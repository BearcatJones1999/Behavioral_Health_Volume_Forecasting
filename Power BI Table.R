# =====================================================================
# BI upload builder — tidy long table for Power BI
# =====================================================================
# One row per  unit × month × series, in BOTH patient-days and ADC.
#   series:  actuals        (all history)
#            budget         (FY26 only)
#            test_forecast  (FY26 backtest: train <= FY25, forecast FY26)
#            forecast       (FY27, combo point + 80/95 bands)
#
# test_forecast is the honest holdout: it only saw data through FY25, the
# same cutoff the budget was locked on, so the FY26 credibility page can
# compare actuals vs budget vs forecast on equal footing. forecast (FY27)
# is trained on all history.
#
# Every unit is forecast the same way (combo). Capacity is carried as an
# INFORMATIONAL column only — NA for units without beds, so partial/bedless
# units can't distort anything. No realized / min() logic. Capacity-change
# and short-history units are flagged in the metadata for manual review.
#
# Assumes vol, fit_models(), CONFIG, CAPACITY_CHANGE_UNITS are loaded.
# Output: bi_forecast_long.csv (BI source) + bi_forecast_long.xlsx (w/ dict)
# =====================================================================

library(fpp3)
library(tidyverse)
library(openxlsx)

flagged <- as.integer(names(CAPACITY_CHANGE_UNITS))

# ---------------------------------------------------------------------
# 1. Per-unit capacity info (informational; NA where the unit has no beds)
# ---------------------------------------------------------------------
cap_month <- function(cap) if_else(cap > 0 & !is.na(cap), cap, NA_real_)

has_beds_tbl <- vol |>
  as_tibble() |>
  group_by(unit) |>
  summarise(has_beds = any(capacity > 0 & !is.na(capacity)), .groups = "drop")

capacity_fwd <- vol |>
  as_tibble() |>
  group_by(unit) |>
  slice_max(ym, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(unit, capacity_fwd = cap_month(capacity))

# per-month capacity for the FY26 holdout (used by the test_forecast rows)
holdout_cap <- vol |>
  as_tibble() |>
  filter(fiscal_year == CONFIG$holdout_fy) |>
  transmute(unit, month_date = as.Date(ym), cap = cap_month(capacity))

# ---------------------------------------------------------------------
# 2. Forecasts (combo point + bands)
#    - test_forecast: train <= FY25, forecast FY26 (blind holdout)
#    - forecast     : train on all history, forecast FY27
# ---------------------------------------------------------------------
extract_combo <- function(fc) {
  fc |>
    filter(.model == "combo") |>
    hilo(level = c(80, 95)) |>
    unpack_hilo(c("80%", "95%")) |>
    as_tibble() |>
    transmute(unit, ym,
              value_pd = .mean,
              lo80_pd = `80%_lower`, hi80_pd = `80%_upper`,
              lo95_pd = `95%_lower`, hi95_pd = `95%_upper`)
}

test_pts <- fit_models(vol |> filter(fiscal_year < CONFIG$holdout_fy)) |>
  forecast(h = CONFIG$horizon) |>
  extract_combo() |>
  mutate(month_date = as.Date(ym), fiscal_year = CONFIG$holdout_fy)

forecast_pts <- fit_models(vol) |>
  forecast(h = CONFIG$horizon) |>
  extract_combo() |>
  mutate(month_date = as.Date(ym), fiscal_year = CONFIG$forecast_fy)

# ---------------------------------------------------------------------
# 3. Series blocks (common schema; bands NA except the forecast series)
# ---------------------------------------------------------------------
na_bands <- function(df) df |>
  mutate(lo80_pd = NA_real_, hi80_pd = NA_real_,
         lo95_pd = NA_real_, hi95_pd = NA_real_)

hist_rows <- vol |>
  as_tibble() |>
  transmute(unit, fiscal_year,
            month_date = as.Date(ym),
            days_in_month = lubridate::days_in_month(month_date),
            series = "actuals", value_pd = actuals,
            capacity_beds = cap_month(capacity)) |>
  na_bands()

budget_rows <- vol |>
  as_tibble() |>
  filter(fiscal_year == CONFIG$holdout_fy, !is.na(budget)) |>
  transmute(unit, fiscal_year,
            month_date = as.Date(ym),
            days_in_month = lubridate::days_in_month(month_date),
            series = "budget", value_pd = budget,
            capacity_beds = cap_month(capacity)) |>
  na_bands()

test_rows <- test_pts |>
  left_join(holdout_cap, by = c("unit", "month_date")) |>
  transmute(unit, fiscal_year, month_date,
            days_in_month = lubridate::days_in_month(month_date),
            series = "test_forecast",
            value_pd, lo80_pd, hi80_pd, lo95_pd, hi95_pd,
            capacity_beds = cap)

forecast_rows <- forecast_pts |>
  left_join(capacity_fwd, by = "unit") |>
  transmute(unit, fiscal_year, month_date,
            days_in_month = lubridate::days_in_month(month_date),
            series = "forecast",
            value_pd, lo80_pd, hi80_pd, lo95_pd, hi95_pd,
            capacity_beds = capacity_fwd)

# ---------------------------------------------------------------------
# 4. Per-unit method / confidence + review notes
# ---------------------------------------------------------------------
min_months <- CONFIG$backtest_init + CONFIG$horizon   # backtestable threshold

# FY27 vs FY26 level jump (same idea as the main-script sanity check)
fy26_level <- vol |>
  filter(fiscal_year == CONFIG$holdout_fy) |>
  as_tibble() |>
  group_by(unit) |>
  summarise(fy26_mean = mean(actuals), .groups = "drop")

fy27_level <- forecast_pts |>
  group_by(unit) |>
  summarise(fy27_mean = mean(value_pd), .groups = "drop")

# FY26 backtest accuracy (test_forecast vs actuals) per unit — matches the
# combo_vs_budget scorecard on the credibility page
test_acc <- test_pts |>
  select(unit, month_date, test_pd = value_pd) |>
  left_join(
    vol |> as_tibble() |>
      filter(fiscal_year == CONFIG$holdout_fy) |>
      transmute(unit, month_date = as.Date(ym), actuals, budget),
    by = c("unit", "month_date")
  ) |>
  group_by(unit) |>
  summarise(
    test_MAPE   = round(mean(abs((actuals - test_pd) / actuals)) * 100, 1),
    budget_MAPE = round(mean(abs((actuals - budget)  / actuals)) * 100, 1),
    .groups = "drop"
  ) |>
  mutate(test_beats_budget = test_MAPE < budget_MAPE)

unit_meta <- vol |>
  as_tibble() |>
  count(unit, name = "n_months") |>
  left_join(has_beds_tbl, by = "unit") |>
  left_join(capacity_fwd, by = "unit") |>
  left_join(fy26_level, by = "unit") |>
  left_join(fy27_level, by = "unit") |>
  left_join(test_acc, by = "unit") |>
  mutate(
    is_capacity_change = unit %in% flagged,
    pct_change = round((fy27_mean / fy26_mean - 1) * 100, 1),
    forecast_confidence = if_else(n_months < min_months, "provisional", "standard"),
    forecast_method     = if_else(n_months < min_months,
                                  "combo (short history, no backtest)",
                                  "combo (all history)"),
    note = str_trim(str_squish(paste(
      if_else(is_capacity_change, "capacity change - review;", ""),
      if_else(!has_beds,          "no beds (capacity n/a);",   ""),
      if_else(abs(pct_change) > CONFIG$sanity_pct,
              paste0("FY27 ", pct_change, "% vs FY26 - review;"), "")
    )))
  ) |>
  select(unit, n_months, has_beds, capacity_fwd, pct_change,
         test_MAPE, budget_MAPE, test_beats_budget,
         is_capacity_change, forecast_confidence, forecast_method, note)

# ---------------------------------------------------------------------
# 5. Assemble long table: add ADC space, capacity_pd/adc, labels, meta
# ---------------------------------------------------------------------
bi_long <- bind_rows(hist_rows, budget_rows, test_rows, forecast_rows) |>
  mutate(
    is_forecast = series %in% c("forecast", "test_forecast"),
    month_name  = format(month_date, "%b"),
    # ADC (beds) space — independent of capacity, valid for every unit
    value_adc = value_pd / days_in_month,
    lo80_adc  = lo80_pd  / days_in_month,
    hi80_adc  = hi80_pd  / days_in_month,
    lo95_adc  = lo95_pd  / days_in_month,
    hi95_adc  = hi95_pd  / days_in_month,
    # capacity space — NA carries through for bedless units
    capacity_pd  = capacity_beds * days_in_month,
    capacity_adc = capacity_beds
  ) |>
  left_join(unit_meta |> select(unit, forecast_method, forecast_confidence, note),
            by = "unit") |>
  mutate(
    across(c(value_pd, lo80_pd, hi80_pd, lo95_pd, hi95_pd, capacity_pd),
           \(x) round(x, 0)),
    across(c(value_adc, lo80_adc, hi80_adc, lo95_adc, hi95_adc, capacity_adc),
           \(x) round(x, 1))
  ) |>
  arrange(unit, month_date,
          factor(series, levels = c("actuals", "budget",
                                    "test_forecast", "forecast"))) |>
  select(unit, fiscal_year, month_date, month_name, days_in_month,
         series, is_forecast,
         value_pd, lo80_pd, hi80_pd, lo95_pd, hi95_pd,
         value_adc, lo80_adc, hi80_adc, lo95_adc, hi95_adc,
         capacity_beds, capacity_pd, capacity_adc,
         forecast_method, forecast_confidence, note)

# ---------------------------------------------------------------------
# 6. Data dictionary
# ---------------------------------------------------------------------
dictionary <- tribble(
  ~column,                ~description,
  "unit",                 "Cost-center / unit ID (key).",
  "fiscal_year",          "Fiscal year; FY starts July (FY26 = Jul25-Jun26).",
  "month_date",           "First of month; primary date axis.",
  "month_name",           "Abbreviated month label.",
  "days_in_month",        "Calendar days (Feb-aware); links patient-days <-> ADC.",
  "series",               "actuals | budget | test_forecast (FY26 backtest) | forecast (FY27).",
  "is_forecast",          "TRUE for test_forecast and forecast rows.",
  "value_pd",             "Value in patient-days.",
  "lo80_pd/hi80_pd",      "80% interval (patient-days); forecast series only.",
  "lo95_pd/hi95_pd",      "95% interval (patient-days); forecast series only.",
  "value_adc",            "Value in ADC = value_pd / days_in_month.",
  "lo80_adc..hi95_adc",   "Intervals in ADC; forecast series only.",
  "capacity_beds",        "Capacity (beds / max ADC) that month; NA if unit has no beds.",
  "capacity_pd",          "Capacity in patient-days = capacity_beds * days; NA if no beds.",
  "capacity_adc",         "Capacity in ADC (= capacity_beds); NA if no beds.",
  "forecast_method",      "How this unit's FY27 forecast was produced.",
  "forecast_confidence",  "standard | provisional.",
  "note",                 "Review flags: capacity change, no beds, large FY27 jump."
)

# ---------------------------------------------------------------------
# 7. Write outputs + quick summary
# ---------------------------------------------------------------------
write_csv(bi_long, "bi_forecast_long.csv")
write.xlsx(
  list(BI_Long = bi_long, Data_Dictionary = dictionary, Unit_Meta = unit_meta),
  file = "bi_forecast_long.xlsx", overwrite = TRUE, asTable = TRUE
)

cat("Wrote bi_forecast_long.csv / .xlsx to:", getwd(), "\n")
cat("Rows:", nrow(bi_long), "| Units:", n_distinct(bi_long$unit), "\n")
cat("Bedless units:",
    paste(unit_meta$unit[!unit_meta$has_beds], collapse = ", "), "\n")
cat("FY26 test beat budget on",
    sum(unit_meta$test_beats_budget, na.rm = TRUE), "of",
    nrow(unit_meta), "units\n")
print(unit_meta, n = Inf)