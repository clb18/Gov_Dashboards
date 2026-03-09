# tests/testthat/test-forecast.R
#
# Unit tests for R/forecast_nextq.R
#
# Tests verify that forecast_next_quarter():
#   1. Returns a tibble with the correct columns
#   2. Returns exactly 3 rows (one per forecast month/quarter)
#   3. Has dates that follow sequentially after the last data point
#   4. Has confidence bands that are ordered correctly (lo < mean < hi)
#   5. Works on monthly and quarterly input data
#   6. Fails gracefully when the 'forecast' package is not installed
#      (this case is tested with a skip if 'forecast' is present)
#
# Run via: source("tests/run_tests.R")

# Skip all forecast tests if the 'forecast' package isn't installed
# (developer machines without it should skip cleanly, not error)
skip_if_no_forecast <- function() {
  testthat::skip_if_not_installed("forecast")
}

# ── Column structure ──────────────────────────────────────────────────────────
testthat::test_that("forecast_next_quarter() returns correct columns", {
  skip_if_no_forecast()

  df_input <- dplyr::select(mock_cpi(), date, value)
  suppressMessages({
    fc <- forecast_next_quarter(df_input)
  })

  testthat::expect_s3_class(fc, "data.frame")
  testthat::expect_named(fc, c("date", "mean", "lo80", "hi80", "lo95", "hi95"),
                         ignore.order = FALSE)
})

# ── Row count ─────────────────────────────────────────────────────────────────
testthat::test_that("forecast_next_quarter() returns exactly 3 rows", {
  skip_if_no_forecast()

  df_input <- dplyr::select(mock_unrate(), date, value)
  suppressMessages({
    fc <- forecast_next_quarter(df_input)
  })

  testthat::expect_equal(nrow(fc), 3L)
})

# ── Forecast dates follow the data ───────────────────────────────────────────
testthat::test_that("forecast dates are strictly after the last data date", {
  skip_if_no_forecast()

  df_input  <- dplyr::select(mock_cpi(), date, value)
  last_date <- max(df_input$date)

  suppressMessages({
    fc <- forecast_next_quarter(df_input)
  })

  testthat::expect_true(all(fc$date > last_date))
})

testthat::test_that("forecast dates are consecutive months", {
  skip_if_no_forecast()

  df_input <- dplyr::select(mock_cpi(), date, value)
  suppressMessages({
    fc <- forecast_next_quarter(df_input)
  })

  gaps <- as.numeric(diff(fc$date))
  testthat::expect_true(all(gaps >= 28 & gaps <= 31))
})

# ── Confidence band ordering ──────────────────────────────────────────────────
testthat::test_that("confidence bands are correctly ordered: lo95 <= lo80 <= mean <= hi80 <= hi95", {
  skip_if_no_forecast()

  df_input <- dplyr::select(mock_cpi(), date, value)
  suppressMessages({
    fc <- forecast_next_quarter(df_input)
  })

  testthat::expect_true(all(fc$lo95 <= fc$lo80))
  testthat::expect_true(all(fc$lo80 <= fc$mean))
  testthat::expect_true(all(fc$mean <= fc$hi80))
  testthat::expect_true(all(fc$hi80 <= fc$hi95))
})

# ── Works on quarterly input ──────────────────────────────────────────────────
testthat::test_that("forecast_next_quarter() works on quarterly GDP data", {
  skip_if_no_forecast()

  df_input <- dplyr::select(mock_gdp(), date, value)
  suppressMessages({
    fc <- forecast_next_quarter(df_input)
  })

  testthat::expect_s3_class(fc, "data.frame")
  testthat::expect_equal(nrow(fc), 3L)
})

# ── Works on rates data ───────────────────────────────────────────────────────
testthat::test_that("forecast_next_quarter() works on Fed Funds Rate series", {
  skip_if_no_forecast()

  df_input <- mock_rates() |>
    dplyr::filter(series_id == "DFF") |>
    dplyr::select(date, value)

  suppressMessages({
    fc <- forecast_next_quarter(df_input)
  })

  testthat::expect_equal(nrow(fc), 3L)
  testthat::expect_false(anyNA(fc$mean))
})
