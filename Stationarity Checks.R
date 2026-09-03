# =====================================================================
# Stationarity + differencing + auto.arima  -- ALL psych units
# Classical Box-Jenkins workup (ADF, KPSS, ndiffs/nsdiffs) plus
# auto.arima, looped over every unit into one summary table.
# Train period only (<= FY25); FY26 held out.
# =====================================================================

setwd("C:/Users/Jonbf5/OneDrive - cchmc/Forecasting Research/Psych Units")

library(fpp3)
library(tidyverse)
library(readxl)
library(tseries)    # adf.test, kpss.test
library(urca)       # ur.df (detailed joint stats)
library(forecast)   # ndiffs, nsdiffs, auto.arima, arimaorder

# ---------------------------------------------------------------------
# 1. Read + shape into a tsibble
# ---------------------------------------------------------------------
df <- read_excel("data.xlsx", sheet = "Sheet2")

vol <- df |>
  rename(unit        = Unit,
         fiscal_year = `Fiscal Year`,
         month       = Month,
         actuals     = Actuals,
         capacity    = Capacity,
         budget      = Budget) |>
  mutate(
    month_clean = str_to_title(str_trim(month)),
    month_num   = match(month_clean, month.name),
    cal_year    = if_else(month_num >= 7, fiscal_year - 1L, fiscal_year),
    ym          = make_yearmonth(year = cal_year, month = month_num)
  ) |>
  as_tsibble(index = ym, key = unit)

stopifnot(sum(is.na(vol$ym)) == 0)   # all dates parsed
print(has_gaps(vol))                  # want all FALSE

# ---------------------------------------------------------------------
# 2. Helper: pull one unit's TRAIN series as a monthly ts object
# ---------------------------------------------------------------------
get_ts <- function(u) {
  vol |>
    filter(unit == u, fiscal_year <= 2025) |>
    as_tibble() |>
    arrange(ym) |>
    pull(actuals) |>
    ts(frequency = 12, start = c(2016, 7))   # FY2017 begins Jul 2016
}

units <- sort(unique(vol$unit))

# ---------------------------------------------------------------------
# 3. Loop: run the full workup on every unit, collect a summary row.
#    adf.test  : H0 = non-stationary  -> small p means STATIONARY
#    kpss.test : H0 = stationary      -> small p means NON-stationary
#    ndiffs/nsdiffs give the (d, D) differencing the tests imply.
# ---------------------------------------------------------------------
summ <- map_dfr(units, function(u) {
  y  <- get_ts(u)
  dy <- diff(y)                      # first difference
  
  # suppress the boundary p-value warnings adf/kpss throw at extremes
  adf_lvl  <- suppressWarnings(adf.test(y)$p.value)
  kpss_lvl <- suppressWarnings(kpss.test(y,  null = "Level")$p.value)
  adf_diff <- suppressWarnings(adf.test(dy)$p.value)
  
  d  <- ndiffs(y)                    # non-seasonal differences suggested
  D  <- tryCatch(nsdiffs(y), error = function(e) NA_integer_)  # seasonal
  
  # auto.arima (full search; slower but honest). Seasonal handled via freq 12.
  fit <- auto.arima(y, stepwise = FALSE, approximation = FALSE)
  ord <- arimaorder(fit)             # named: p d q P D Q Frequency
  
  tibble(
    unit          = u,
    adf_p_level   = round(adf_lvl,  3),
    kpss_p_level  = round(kpss_lvl, 3),
    stationary_lvl = adf_lvl < 0.05 & kpss_lvl > 0.05,   # both agree = clean
    ndiffs_d      = d,
    nsdiffs_D     = D,
    adf_p_diff    = round(adf_diff, 3),                  # after 1 difference
    auto_p = ord["p"], auto_d = ord["d"], auto_q = ord["q"],
    auto_P = ifelse(length(ord) > 3, ord["P"], NA),
    auto_D = ifelse(length(ord) > 3, ord["D"], NA),
    auto_Q = ifelse(length(ord) > 3, ord["Q"], NA)
  )
})

print(summ, n = Inf, width = Inf)

# ---------------------------------------------------------------------
# 4. Reconcile: does auto.arima's (d, D) match the ndiffs/nsdiffs tests?
#    A mismatch isn't necessarily wrong -- auto.arima minimizes AICc and
#    can override the unit-root test -- but it's worth eyeballing.
# ---------------------------------------------------------------------
summ |>
  transmute(unit,
            test_d = ndiffs_d, auto_d,
            test_D = nsdiffs_D, auto_D,
            d_match = ndiffs_d == auto_d,
            D_match = nsdiffs_D == auto_D) |>
  print(n = Inf)

# ---------------------------------------------------------------------
# 5. OPTIONAL detailed view for a single unit (classical joint stats
#    + ACF/PACF before and after differencing). Change u and re-run.
# ---------------------------------------------------------------------
u <- 304400
y <- get_ts(u)

# ur.df with the full tau/phi joint statistics (as in ECON8011 scripts)
summary(ur.df(y, type = "trend", selectlags = "BIC"))
summary(ur.df(y, type = "drift", selectlags = "BIC"))

ggtsdisplay(y,        main = paste("Unit", u, "- level"))
ggtsdisplay(diff(y),  main = paste("Unit", u, "- first difference"))

# what auto.arima settled on, for this unit
summary(auto.arima(y, stepwise = FALSE, approximation = FALSE))

# ---------------------------------------------------------------------
# 6. OPTIONAL cross-check vs fable's ARIMA() (should broadly agree with
#    auto.arima; both search around the same unit-root logic).
# ---------------------------------------------------------------------
vol |>
  filter(fiscal_year <= 2025) |>
  model(fable_arima = ARIMA(actuals)) |>
  mutate(spec = format(fable_arima)) |>
  as_tibble() |>
  select(unit, spec) |>
  print(n = Inf)
