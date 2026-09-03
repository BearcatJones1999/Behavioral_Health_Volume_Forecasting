# =====================================================================
# Behavioral Health (Psych) Units — FY27 Volume Forecast
# =====================================================================
# Purpose : Forecast monthly volume (patient-days) per inpatient psych
#           unit, validate against FY26 actuals + budget, and export an
#           FY27 forecast (Jul 2026 – Jun 2027) with ADC + intervals.
#
# Input   : data.xlsx / Sheet2 — columns: Unit, Fiscal Year, Month,
#           Actuals, Capacity, Budget  (Budget filled for FY26 only).
# Output  : FY27_forecast.xlsx — FY27_Forecast + Sanity_Check sheets.
#
# Pipeline:
#   1. Config            — all knobs in one place
#   2. Helpers           — fit_models(), budget_accuracy()
#   3. Load + shape      — tsibble keyed by unit, indexed by yearmonth
#   4. Integrity checks
#   5. EDA + diagnostics — interactive only; not on the export path
#   6. Rolling-origin backtest
#   7. FY26 head-to-head — models vs seasonal-naive vs BUDGET
#   8. Capacity check    — units with a bed change (e.g. 304400)
#   9. FY27 forecast + Excel export
#
# Models  : snaive (benchmark), arima, ets, combo = mean(arima,ets,snaive)
# Notes   : Fiscal year starts in July (FY26 = Jul 2025 – Jun 2026).
#           Error sign: error = actual - predicted, so ME > 0 means the
#           forecast ran LOW (under-predicted).
# =====================================================================

#library(fpp3)
#library(tidyverse)
#library(readxl)
#library(openxlsx)

# ---------------------------------------------------------------------
# 1. Config
# ---------------------------------------------------------------------
#setwd("C:/Users/Jonbf5/OneDrive - cchmc/Forecasting Research/Psych Units")

CONFIG <- list(
  data_file     = "data.xlsx",
  data_sheet    = "Sheet2",
  holdout_fy    = 2026L,   # FY with actuals + budget for the head-to-head
  forecast_fy   = 2027L,   # FY to produce
  horizon       = 12L,     # forecast / backtest horizon (annual cadence)
  backtest_init = 60L,     # months in first backtest training window
  backtest_step = 12L,     # step between origins (lands on FY boundaries)
  sanity_pct    = 15,      # flag FY27 units that jump > this % vs FY26
  output_file   = "FY27_forecast.xlsx"
)

# Units with a known capacity change -> capacity_check() derives the
# ratio (holdout / train median capacity) from the data, never hardcoded.
CAPACITY_CHANGE_UNITS <- c(`304400` = "24 -> 16 beds")

# ---------------------------------------------------------------------
# 2. Helpers
# ---------------------------------------------------------------------
# Standard model set (+ combo) fit on any tsibble slice.
fit_models <- function(.data) {
  .data |>
    model(
      snaive = SNAIVE(actuals),
      arima  = ARIMA(actuals),
      ets    = ETS(actuals)
    ) |>
    mutate(combo = (arima + ets + snaive) / 3)
}

# Budget scored with the SAME metric definitions as accuracy().
budget_accuracy <- function(holdout) {
  holdout |>
    as_tibble() |>
    group_by(unit) |>
    summarise(
      .model = "budget",
      ME   = mean(actuals - budget),
      MAE  = mean(abs(actuals - budget)),
      RMSE = sqrt(mean((actuals - budget)^2)),
      MAPE = mean(abs((actuals - budget) / actuals)) * 100,
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------
# 3. Load + shape
# ---------------------------------------------------------------------
vol <- read_excel(CONFIG$data_file, sheet = CONFIG$data_sheet) |>
  rename(
    unit        = Unit,
    fiscal_year = `Fiscal Year`,
    month       = Month,
    actuals     = Actuals,
    capacity    = Capacity,
    budget      = Budget
  ) |>
  mutate(
    month_num = match(str_to_title(str_trim(month)), month.name),
    cal_year  = if_else(month_num >= 7, fiscal_year - 1L, fiscal_year),
    ym        = make_yearmonth(year = cal_year, month = month_num)
  ) |>
  as_tsibble(index = ym, key = unit)   # budget rides along; models ignore it

# ---------------------------------------------------------------------
# 4. Integrity checks  (must pass before modeling)
# ---------------------------------------------------------------------
stopifnot(
  "NA in date index"       = sum(is.na(vol$ym)) == 0,
  "gaps in monthly series" = all(!has_gaps(vol)$.gaps)
)

# budget present for the holdout FY and NA elsewhere
vol |>
  as_tibble() |>
  group_by(fiscal_year) |>
  summarise(n = n(), budget_filled = sum(!is.na(budget)), .groups = "drop") |>
  print(n = Inf)

# ---------------------------------------------------------------------
# 5. EDA + residual diagnostics  (interactive only — off the export path)
# ---------------------------------------------------------------------
# Per-unit history with the FY26 holdout marker.
plot_unit <- function(u) {
  vol |>
    filter(unit == u) |>
    autoplot(actuals) +
    geom_vline(xintercept = as.Date(yearmonth("2025 Jul")),
               linetype = "dashed", colour = "red") +
    labs(title = paste0(u, " — full history"),
         subtitle = "Red line = FY26 start (budget/holdout year)",
         x = NULL, y = "actuals")
}

if (interactive()) {
  # all units at a glance
  vol |> autoplot(actuals) + facet_wrap(vars(unit), scales = "free_y")
  
  # single-unit deep dives
  # plot_unit(304400)                 # clean trending unit
  # plot_unit(304607)                 # deepest COVID dip
  
  # residual diagnostics on the fitted models
  fit_dx <- fit_models(vol)
  # fit_dx |> filter(unit == 304400) |> report()
  # fit_dx |> select(unit, arima) |> filter(unit == 304607) |> gg_tsresiduals()
  
  # 304600: is seasonality real but swamped? STL vs forced seasonal ARIMA
  # vol |> filter(unit == 304600) |> model(STL(actuals)) |> components() |> autoplot()
  # vol |> filter(fiscal_year <= 2025, unit == 304600) |>
  #   model(auto = ARIMA(actuals),
  #         forced_s = ARIMA(actuals ~ 0 + pdq(1,1,1) + PDQ(1,0,0))) |>
  #   forecast(h = 12) |> autoplot(vol |> filter(unit == 304600), level = NULL)
}

# ---------------------------------------------------------------------
# 6. Rolling-origin backtest
#    stretch_tsibble needs every series longer than .init, so restrict
#    to units with enough history for at least one scoreable h=12 fold.
#    Shorter units are reported and skipped (still forecast in 8 & 9).
# ---------------------------------------------------------------------
min_months <- CONFIG$backtest_init + CONFIG$horizon   # need init + one test window

unit_len <- vol |>
  as_tibble() |>
  count(unit, name = "n_months")

bt_units   <- unit_len |> filter(n_months >= min_months) |> pull(unit)
skip_units <- unit_len |> filter(n_months <  min_months)

if (nrow(skip_units) > 0) {
  cat("Skipping backtest for units with <", min_months, "months:\n")
  print(skip_units)
}

fc_cv <- vol |>
  filter(unit %in% bt_units) |>
  stretch_tsibble(.init = CONFIG$backtest_init, .step = CONFIG$backtest_step) |>
  fit_models() |>
  forecast(h = CONFIG$horizon)

# pooled across all scoreable test months
fc_cv |>
  accuracy(vol) |>
  select(unit, .model, MASE, RMSE, MAPE) |>
  arrange(unit, .model) |>
  print(n = Inf)

# by fold — does the ARIMA edge hold on the recent, clean years?
fc_cv |>
  accuracy(vol, by = c("unit", ".model", ".id")) |>
  select(unit, .model, .id, MASE, MAPE) |>
  arrange(unit, .id, .model) |>
  print(n = Inf)

# ---------------------------------------------------------------------
# 7. FY26 head-to-head: models vs seasonal-naive vs BUDGET
#    Train < FY26, forecast FY26, score every model AND budget vs actuals.
#    Budget was locked at FY25 year-end on the same information -> fair.
# ---------------------------------------------------------------------
train   <- vol |> filter(fiscal_year <  CONFIG$holdout_fy)
holdout <- vol |> filter(fiscal_year == CONFIG$holdout_fy)

stopifnot(
  "missing budget in holdout" =
    holdout |> as_tibble() |> summarise(m = sum(is.na(budget))) |> pull(m) == 0
)

fc_fy26 <- fit_models(train) |> forecast(h = CONFIG$horizon)

model_acc <- fc_fy26 |>
  accuracy(holdout) |>
  select(unit, .model, ME, MAE, RMSE, MAPE)

# stacked leaderboard: every model + budget, best (lowest MAPE) first
leaderboard <- bind_rows(model_acc, budget_accuracy(holdout)) |>
  arrange(unit, MAPE)
print(leaderboard, n = Inf)

# best MODEL per unit lined up beside budget (the fair, per-unit framing)
best_vs_budget <- leaderboard |>
  filter(.model != "budget") |>
  group_by(unit) |>
  slice_min(MAPE, n = 1) |>
  ungroup() |>
  select(unit, model = .model, model_MAPE = MAPE) |>
  left_join(
    leaderboard |> filter(.model == "budget") |> select(unit, budget_MAPE = MAPE),
    by = "unit"
  ) |>
  mutate(model_beats_budget = model_MAPE < budget_MAPE)
print(best_vs_budget, n = Inf)

# visual: FY26 actuals vs budget vs model forecasts
if (interactive()) {
  fc_points_fy26 <- fc_fy26 |>
    as_tibble() |>
    select(unit, ym, .model, .mean) |>
    pivot_wider(names_from = .model, values_from = .mean)
  
  holdout |>
    as_tibble() |>
    select(unit, ym, actuals, budget) |>
    left_join(fc_points_fy26, by = c("unit", "ym")) |>
    pivot_longer(-c(unit, ym), names_to = "series", values_to = "value") |>
    ggplot(aes(ym, value, colour = series)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(vars(unit), scales = "free_y") +
    labs(title = "FY26: actuals vs budget vs model forecasts",
         x = NULL, y = NULL, colour = NULL)
}

# ---------------------------------------------------------------------
# 8. Capacity check (units with a bed change)
#    For each flagged unit: does scaling the FY26 forecast by the
#    capacity ratio (new/old, derived from data) beat the raw forecast?
#    304400 = 24 -> 16 beds: 9 yrs of 24-bed history, then the cut.
# ---------------------------------------------------------------------
capacity_check <- function(u) {
  tr <- vol |> filter(unit == u, fiscal_year <  CONFIG$holdout_fy)
  ho <- vol |> filter(unit == u, fiscal_year == CONFIG$holdout_fy)
  
  ratio <- (ho |> as_tibble() |> summarise(c = median(capacity)) |> pull(c)) /
    (tr |> as_tibble() |> summarise(c = median(capacity)) |> pull(c))
  
  pts <- fit_models(tr) |>
    forecast(h = CONFIG$horizon) |>
    as_tibble() |>
    select(.model, ym, .mean) |>
    left_join(ho |> as_tibble() |> select(ym, actuals, budget), by = "ym") |>
    mutate(scaled = .mean * ratio)
  
  acc <- pts |>
    group_by(.model) |>
    summarise(
      MAPE_raw    = mean(abs((actuals - .mean)  / actuals)) * 100,
      MAPE_scaled = mean(abs((actuals - scaled) / actuals)) * 100,
      .groups = "drop"
    ) |>
    arrange(MAPE_scaled)
  
  budget_mape <- pts |>
    distinct(ym, actuals, budget) |>
    summarise(m = mean(abs((actuals - budget) / actuals)) * 100) |>
    pull(m)
  
  list(unit = u, ratio = ratio, accuracy = acc, budget_MAPE = budget_mape)
}

for (u in names(CAPACITY_CHANGE_UNITS)) {
  res <- capacity_check(as.integer(u))
  cat("\nUnit", res$unit, "—", CAPACITY_CHANGE_UNITS[u],
      "| capacity ratio =", round(res$ratio, 3),
      "| budget MAPE =", round(res$budget_MAPE, 1), "\n")
  print(res$accuracy)
}

# ---------------------------------------------------------------------
# 9. FY27 forecast (train on ALL data) + Excel export
# ---------------------------------------------------------------------
fc_fy27 <- fit_models(vol) |> forecast(h = CONFIG$horizon)

# point forecasts, models side by side
points <- fc_fy27 |>
  as_tibble() |>
  select(unit, ym, .model, .mean) |>
  pivot_wider(names_from = .model, values_from = .mean)

# prediction intervals for the combo (recommended default)
intervals <- fc_fy27 |>
  filter(.model == "combo") |>
  hilo(level = c(80, 95)) |>
  unpack_hilo(c("80%", "95%")) |>
  as_tibble() |>
  select(unit, ym,
         lo80 = `80%_lower`, hi80 = `80%_upper`,
         lo95 = `95%_lower`, hi95 = `95%_upper`)

# assemble export (one row per unit-month) with ADC
export <- points |>
  left_join(intervals, by = c("unit", "ym")) |>
  mutate(
    fiscal_year   = CONFIG$forecast_fy,
    month         = format(ym, "%B"),
    month_date    = as.Date(ym),
    days_in_month = lubridate::days_in_month(month_date),   # Feb-aware
    adc_combo     = combo / days_in_month
  ) |>
  select(unit, fiscal_year, month, month_date, days_in_month,
         snaive, arima, ets, combo, adc_combo,
         lo80, hi80, lo95, hi95) |>
  arrange(unit, month_date) |>
  mutate(
    across(c(snaive, arima, ets, combo, lo80, hi80, lo95, hi95),
           \(x) round(x, 0)),
    adc_combo = round(adc_combo, 1)
  )

# sanity check: FY27 combo level vs FY26 actual level
# (flag units whose forecast jumps > sanity_pct — 304400 is the one to watch,
#  12 post-bed-cut months vs 9 yrs of 24-bed history)
sanity <- vol |>
  filter(fiscal_year == CONFIG$holdout_fy) |>
  as_tibble() |>
  group_by(unit) |>
  summarise(fy26_actual_mean = mean(actuals), .groups = "drop") |>
  left_join(
    export |> group_by(unit) |> summarise(fy27_combo_mean = mean(combo), .groups = "drop"),
    by = "unit"
  ) |>
  mutate(
    pct_change = round((fy27_combo_mean / fy26_actual_mean - 1) * 100, 1),
    flag       = if_else(abs(pct_change) > CONFIG$sanity_pct, "REVIEW", "")
  )
print(sanity, n = Inf)

# write Excel (forecast + sanity check)
write.xlsx(
  list(FY27_Forecast = export, Sanity_Check = sanity),
  file      = CONFIG$output_file,
  overwrite = TRUE,
  asTable   = TRUE
)
cat("Wrote", CONFIG$output_file, "to:", getwd(), "\n")


###Suplement

# =====================================================================
# FY26: combo vs budget — per-unit plot + win/loss table
# Assumes `vol` (shaped tsibble), CONFIG, and fit_models() are loaded.
# =====================================================================

train   <- vol |> filter(fiscal_year <  CONFIG$holdout_fy)
holdout <- vol |> filter(fiscal_year == CONFIG$holdout_fy)

combo_fc <- fit_models(train) |>
  forecast(h = CONFIG$horizon) |>
  filter(.model == "combo") |>
  as_tibble() |>
  select(unit, ym, combo = .mean)

# ---- table: did combo beat budget? (lower MAPE wins) ----
combo_vs_budget <- holdout |>
  as_tibble() |>
  select(unit, ym, actuals, budget) |>
  left_join(combo_fc, by = c("unit", "ym")) |>
  group_by(unit) |>
  summarise(
    combo_MAPE  = mean(abs((actuals - combo)  / actuals)) * 100,
    budget_MAPE = mean(abs((actuals - budget) / actuals)) * 100,
    .groups = "drop"
  ) |>
  mutate(
    winner    = if_else(combo_MAPE < budget_MAPE, "combo", "budget"),
    margin_pp = round(budget_MAPE - combo_MAPE, 1),   # + => combo better
    across(c(combo_MAPE, budget_MAPE), \(x) round(x, 1))
  )

print(combo_vs_budget, n = Inf)
cat("combo beat budget in",
    sum(combo_vs_budget$winner == "combo"), "of",
    nrow(combo_vs_budget), "units\n")

# ---- plot: budget vs combo per unit (actuals = reference line) ----
holdout |>
  as_tibble() |>
  select(unit, ym, actuals, budget) |>
  left_join(combo_fc, by = c("unit", "ym")) |>
  pivot_longer(c(actuals, budget, combo),
               names_to = "series", values_to = "value") |>
  ggplot(aes(ym, value, colour = series, linewidth = series)) +
  geom_line() +
  facet_wrap(vars(unit), scales = "free_y") +
  scale_colour_manual(values = c(actuals = "black",
                                 budget  = "#d1495b",
                                 combo   = "#3b8ea5")) +
  scale_linewidth_manual(values = c(actuals = 1.1, budget = 0.7, combo = 0.7),
                         guide = "none") +
  labs(title = "FY26: combo vs budget  (actuals = black reference)",
       x = NULL, y = NULL, colour = NULL)

