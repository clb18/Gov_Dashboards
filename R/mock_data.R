# R/mock_data.R
#
# Generates realistic synthetic data for all four dashboard topics:
#   mock_cpi()    → Inflation (CPI-U, monthly index values)
#   mock_unrate() → Labor / Unemployment Rate (monthly %)
#   mock_gdp()    → GDP growth rate (quarterly %, annualized)
#   mock_rates()  → Interest rates: DFF, DGS2, DGS10 (monthly %)
#
# Every function returns a tibble in the SAME format as the real parse functions:
#   date <date> | value <dbl> | series_id <chr> | label <chr> | freq <chr>
#
# HOW TO SWITCH TO LIVE DATA:
#   1. Add your API key(s) to GitHub Secrets (Settings → Secrets → Actions).
#   2. In each dashboard .qmd, change:
#        USE_MOCK <- TRUE
#      to:
#        USE_MOCK <- Sys.getenv("YOUR_KEY_NAME") == ""
#   No other code changes are needed — the parse functions accept the same output.
#
# NOTE: set.seed() is called inside each function so results are reproducible
# and consistent across renders.

# ── CPI / Inflation ───────────────────────────────────────────────────────────

#' Mock CPI-U All Items (Seasonally Adjusted) — BLS series CUSR0000SA0
#'
#' Simulates a monthly price index starting near 130 in 1990 and growing to
#' roughly 315+ by 2026, including a realistic 2021-2023 inflation spike.
#'
#' @param start_date Character "YYYY-MM-DD". Default "1990-01-01".
#' @return Tibble: date, value (index), series_id, label, freq = "M".
mock_cpi <- function(start_date = "1990-01-01") {
  set.seed(101)

  dates <- seq(as.Date(start_date), Sys.Date(), by = "month")
  n     <- length(dates)

  # Base monthly growth ~0.25% (≈3% annual) plus a 2021-2023 inflation spike
  base_trend  <- 0.0025
  spike_idx   <- which(dates >= as.Date("2021-01-01") &
                         dates <= as.Date("2023-06-01"))
  shocks      <- rep(0, n)
  shocks[spike_idx] <- 0.004   # additional ~5% annualized during spike

  monthly_chg <- base_trend + shocks + stats::rnorm(n, mean = 0, sd = 0.001)
  index_vals  <- 130 * cumprod(1 + monthly_chg)   # base ≈ 130 in Jan 1990

  tibble::tibble(
    date      = dates,
    value     = round(index_vals, 3),
    series_id = "CUSR0000SA0",
    label     = "CPI-U (All Items, SA)",
    freq      = "M"
  )
}

# ── Unemployment Rate ─────────────────────────────────────────────────────────

#' Mock Unemployment Rate (Seasonally Adjusted) — BLS series LNS14000000
#'
#' Simulates monthly U-3 unemployment including the 2008-2009 recession rise,
#' recovery, 2020 COVID spike, and subsequent return toward historical lows.
#'
#' @param start_date Character "YYYY-MM-DD". Default "1990-01-01".
#' @return Tibble: date, value (percent), series_id, label, freq = "M".
mock_unrate <- function(start_date = "1990-01-01") {
  set.seed(202)

  dates    <- seq(as.Date(start_date), Sys.Date(), by = "month")
  n        <- length(dates)
  values   <- numeric(n)
  values[1] <- 5.5

  for (i in seq(2, n)) {
    d <- dates[i]

    if (d >= as.Date("2020-03-01") && d <= as.Date("2020-05-01")) {
      # COVID-19 sudden spike (March-May 2020)
      values[i] <- values[i - 1] + stats::runif(1, 2.5, 5.0)

    } else if (d >= as.Date("2020-06-01") && d <= as.Date("2022-06-01")) {
      # Rapid post-COVID recovery
      values[i] <- max(3.5, values[i - 1] - stats::runif(1, 0.2, 0.5))

    } else if (d >= as.Date("2008-09-01") && d <= as.Date("2010-01-01")) {
      # Great Financial Crisis rise
      values[i] <- values[i - 1] + stats::runif(1, 0.1, 0.4)

    } else if (d >= as.Date("2010-02-01") && d <= as.Date("2015-12-01")) {
      # Slow post-GFC recovery
      values[i] <- max(4.5, values[i - 1] - stats::runif(1, 0.0, 0.15))

    } else {
      # Normal slow drift with small random fluctuations
      values[i] <- values[i - 1] + stats::rnorm(1, mean = -0.03, sd = 0.12)
    }
  }

  # Clamp to realistic bounds [3.0%, 15.0%]
  values <- pmin(pmax(values, 3.0), 15.0)

  tibble::tibble(
    date      = dates,
    value     = round(values, 1),
    series_id = "LNS14000000",
    label     = "Unemployment Rate (SA)",
    freq      = "M"
  )
}

# ── GDP Growth Rate ───────────────────────────────────────────────────────────

#' Mock Real GDP Growth (Annualized % Change) — BEA series A191RL1Q225SBEA
#'
#' Simulates quarterly real GDP growth with plausible recession troughs and
#' expansion peaks, including the 2020 COVID collapse and rebound.
#'
#' @param start_date Character "YYYY-MM-DD". Default "1990-01-01".
#' @return Tibble: date (first month of quarter), value (annualized %),
#'   series_id, label, freq = "Q".
mock_gdp <- function(start_date = "1990-01-01") {
  set.seed(303)

  dates    <- seq(as.Date(start_date), Sys.Date(), by = "quarter")
  n        <- length(dates)
  values   <- numeric(n)
  values[1] <- 2.5

  for (i in seq(2, n)) {
    d <- dates[i]

    if (d >= as.Date("2020-04-01") && d <= as.Date("2020-06-30")) {
      # Q2 2020: historic COVID collapse
      values[i] <- -29.9

    } else if (d >= as.Date("2020-07-01") && d <= as.Date("2020-09-30")) {
      # Q3 2020: historic rebound
      values[i] <- 35.3

    } else if (d >= as.Date("2008-10-01") && d <= as.Date("2009-06-30")) {
      # Great Financial Crisis quarters
      values[i] <- stats::rnorm(1, mean = -3.5, sd = 1.2)

    } else if (d >= as.Date("1990-07-01") && d <= as.Date("1991-03-31")) {
      # Early 90s recession
      values[i] <- stats::rnorm(1, mean = -1.0, sd = 0.8)

    } else {
      # Normal expansion: average ~2.3% annualized
      values[i] <- stats::rnorm(1, mean = 2.3, sd = 0.9)
    }
  }

  tibble::tibble(
    date      = dates,
    value     = round(values, 1),
    series_id = "A191RL1Q225SBEA",
    label     = "Real GDP Growth (Annualized %)",
    freq      = "Q"
  )
}

# ── Interest Rates ────────────────────────────────────────────────────────────

#' Mock interest rate series: DFF, DGS2, DGS10.
#'
#' Returns all three series row-bound into one tibble — the same format as
#' get_rates_bundle(). Rates follow a plausible historical path:
#'   high in the 1990s → near-zero post-GFC → near-zero post-COVID →
#'   rapid rise 2022-2023 → gradual decline 2024-present.
#'
#' @param start_date Character "YYYY-MM-DD". Default "1990-01-01".
#' @return Tibble: date, value (percent), series_id, label, freq = "M".
mock_rates <- function(start_date = "1990-01-01") {
  dates <- seq(as.Date(start_date), Sys.Date(), by = "month")
  n     <- length(dates)

  # Internal helper: build a single rate path with regime-aware drift
  make_rate <- function(start_val, seed_offset, term_premium = 0) {
    set.seed(404 + seed_offset)
    v    <- numeric(n)
    v[1] <- start_val + term_premium

    for (i in seq(2, n)) {
      d <- dates[i]

      if (d >= as.Date("2008-12-01") && d <= as.Date("2015-12-01")) {
        # Post-GFC zero lower bound era
        v[i] <- max(0.05, v[i - 1] + stats::rnorm(1, mean = -0.06, sd = 0.05))

      } else if (d >= as.Date("2020-03-01") && d <= as.Date("2021-12-01")) {
        # Post-COVID zero lower bound
        v[i] <- max(0.05, v[i - 1] + stats::rnorm(1, mean = -0.04, sd = 0.04))

      } else if (d >= as.Date("2022-01-01") && d <= as.Date("2023-09-01")) {
        # Rapid tightening cycle
        v[i] <- min(5.5 + term_premium, v[i - 1] + stats::rnorm(1, mean = 0.30, sd = 0.10))

      } else if (d >= as.Date("2023-10-01")) {
        # Gradual easing
        v[i] <- max(3.0 + term_premium * 0.5, v[i - 1] + stats::rnorm(1, mean = -0.08, sd = 0.08))

      } else {
        # Normal drift
        v[i] <- max(0.05, v[i - 1] + stats::rnorm(1, mean = 0, sd = 0.15))
      }
    }
    round(pmax(v, 0.05), 2)
  }

  dplyr::bind_rows(
    tibble::tibble(
      date      = dates,
      value     = make_rate(start_val = 6.5,  seed_offset = 0, term_premium = 0.0),
      series_id = "DFF",
      label     = "Effective Fed Funds Rate",
      freq      = "M"
    ),
    tibble::tibble(
      date      = dates,
      value     = make_rate(start_val = 6.5,  seed_offset = 1, term_premium = 0.3),
      series_id = "DGS2",
      label     = "2Y Treasury",
      freq      = "M"
    ),
    tibble::tibble(
      date      = dates,
      value     = make_rate(start_val = 7.5,  seed_offset = 2, term_premium = 0.8),
      series_id = "DGS10",
      label     = "10Y Treasury",
      freq      = "M"
    )
  )
}
