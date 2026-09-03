# Inpatient Behavioral Health Volume Forecast

Monthly volume forecasting for inpatient behavioral health units, validated
against budget and delivered to a Power BI dashboard for capacity, staffing,
and budget planning.

The pipeline forecasts each unit's monthly volume for the upcoming fiscal year,
back-tests the method on the most recent completed year (train blind, score
against actuals **and** budget), and reshapes the results into a tidy table
Power BI reads directly.

---

## What it does

- Forecasts monthly volume per unit in two spaces: **patient-days** (finance /
  budget) and **average daily census (ADC / beds)** (staffing / capacity).
- Produces **80% and 95% prediction intervals**, not just a point estimate.
- Runs an honest **FY26 holdout test**: models trained only on data through
  FY25, asked to predict FY26 blind, then scored against actuals and against
  budget on equal footing. Consensus matched or beat budget accuracy on
  **8 of 10 units** (pooled 11.9% vs 15.2% MAPE).
- Flags units needing manual review (recent bed change, short history, large
  year-over-year jump) rather than silently trusting them.

---

## Repository structure

```
.
├── data/
│   └── data.xlsx              # Sheet2: Unit, Fiscal Year, Month, Actuals, Capacity, Budget
├── psych_fy27_forecast.R      # main pipeline: fit, backtest, FY26 test, FY27 export
├── build_bi_upload.R          # reshape results into a tidy long table for Power BI
├── outputs/
│   ├── FY27_forecast.xlsx      # per-unit-month point forecast + intervals + ADC
│   ├── bi_forecast_long.csv    # Power BI source (tidy long)
│   └── bi_forecast_long.xlsx   # same + data dictionary + unit metadata
└── README.md
```

---

## Data

Input is a single Excel sheet (`data.xlsx` / `Sheet2`), one row per
unit-month:

| Column        | Description                                              |
|---------------|----------------------------------------------------------|
| `Unit`        | Cost-center / unit ID                                    |
| `Fiscal Year` | Fiscal year (FY starts **July**; FY26 = Jul 2025–Jun 2026) |
| `Month`       | Calendar month name                                      |
| `Actuals`     | Realized volume in patient-days                          |
| `Capacity`    | Beds (max ADC); blank/0 for units without beds           |
| `Budget`      | Budgeted volume — populated for the holdout year only    |

Roughly six years of monthly history per unit. Fiscal months are mapped to a
calendar `yearmonth` index and loaded as a `tsibble` keyed by unit.

---

## Methodology

**Models** (fit per unit):

| Model         | What it captures                                              |
|---------------|--------------------------------------------------------------|
| Seasonal naïve | Repeats last year's same month — the benchmark to beat.     |
| ARIMA         | Trend and month-to-month momentum, selected automatically.   |
| ETS           | Level, trend, and seasonal shape, weighting recent months.   |
| **Consensus** | **Average of the three — the published default.**            |

The consensus (combo) is used because no single method wins on every unit;
averaging cancels individual weaknesses and is the most stable choice overall.

**Validation**

- **Rolling-origin backtest** — expanding training windows stepped by 12 months
  (each origin lands on a fiscal-year boundary), so every fold mimics a real
  one-year-ahead budget forecast. Scored with MASE (vs seasonal-naïve) and MAPE.
- **FY26 head-to-head** — train ≤ FY25, forecast FY26 blind, score models and
  budget against actuals on identical information.

**Key concepts**

- **Patient-days vs ADC** — `ADC = patient_days / days_in_month`. Both are
  carried through so each audience sees the forecast in its own units.
- **Capacity is informational only.** No forecast is derived from bed counts, so
  units without beds can't be distorted. Capacity is available as an overlay line
  for demand-vs-capacity views.
- **Confidence tags** — every unit is labeled `standard`, `provisional` (limited
  history), or flagged for review (recent bed change). Check the tag before
  acting on a unit's numbers.

---

## Usage

Requirements: R (≥ 4.1) and the packages below.

```r
install.packages(c("fpp3", "tidyverse", "readxl", "openxlsx"))
```

Run in order:

```r
# 1. Fit, back-test, validate against FY26, export FY27 forecast
source("psych_fy27_forecast.R")   # -> outputs/FY27_forecast.xlsx

# 2. Reshape into the Power BI upload table (reuses objects from step 1)
source("build_bi_upload.R")       # -> outputs/bi_forecast_long.csv / .xlsx
```

Configuration lives in the `CONFIG` list at the top of `psych_fy27_forecast.R`
(data path, holdout year, forecast year, horizon, backtest window, review
threshold). Units with a known bed change are listed in `CAPACITY_CHANGE_UNITS`.

---

## Outputs

**`FY27_forecast.xlsx`** — per unit-month: each model's point forecast, the
consensus, ADC, 80/95 intervals, and a sanity-check sheet flagging large
year-over-year shifts.

**`bi_forecast_long.csv` / `.xlsx`** — one tidy row per unit × month × series,
in patient-days and ADC, with intervals on the forecast rows and a capacity
column. Series:

| `series`         | Meaning                                             |
|------------------|-----------------------------------------------------|
| `actuals`        | All historical volume                               |
| `budget`         | Budgeted volume (holdout year only)                 |
| `test_forecast`  | FY26 blind backtest (train ≤ FY25)                  |
| `forecast`       | FY27 forecast (trained on all history)              |

The `.xlsx` also ships a data dictionary and a per-unit metadata sheet
(history length, capacity, backtest MAPE vs budget MAPE, review notes).

---

## Power BI

Point Power BI at `bi_forecast_long.csv` for clean refreshes; keep the `.xlsx`
for handoff (it carries the dictionary). Drive the time axis off `month_date`
(a real date) and use `fiscal_year` as a slicer. Dashboard pages:

- **Overview** — methodology, model summary, and history at a glance.
- **FY26 Budget vs Forecast Test** — actuals vs budget vs blind forecast, with a
  per-unit MAPE scorecard (the credibility page).
- **FY27 Forecast** — forward forecast with intervals, in patient-days and ADC.

---

## Notes & caveats

- Forecasts are **decision-support estimates, not commitments.** Budget is a
  target; the forecast is a validated, unbiased anchor to build and measure it
  against.
- Accuracy is demonstrated on a **single holdout year** — strong evidence, not
  proof. Credibility compounds as the track record lengthens.
- Short-history and bed-change units are labeled, not hidden. Read the tags.

## Roadmap

- Extend the same engine to additional units and service lines.
- Quantify the operational value of accuracy (e.g. FY26 staffing cost-of-error,
  budget vs forecast) using cost coefficients from Finance/nursing.
