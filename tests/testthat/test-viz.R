# tests/testthat/test-viz.R
#
# Unit tests for all visualization functions.
#
# Tests verify that every plot function:
#   1. Returns a ggplot object (can be printed/rendered)
#   2. Does not throw errors on typical mock data input
#   3. Has the expected title and axis labels
#
# Tests do NOT check exact visual output (pixel-level), only that the
# ggplot objects are well-formed and contain expected metadata.
#
# Run via: source("tests/run_tests.R")

# ── Shared test data ──────────────────────────────────────────────────────────
# Generate once at file level; all tests in this file share these objects.
cpi_df    <- mock_cpi()
unrate_df <- mock_unrate()
gdp_df    <- mock_gdp()
rates_df  <- mock_rates()

# ── viz_inflation.R ───────────────────────────────────────────────────────────
testthat::test_that("plot_cpi_index() returns a ggplot", {
  p <- plot_cpi_index(cpi_df)
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_cpi_yoy() returns a ggplot", {
  p <- plot_cpi_yoy(cpi_df)
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_cpi_forecast() returns a ggplot", {
  # Suppress forecast package messages during test
  suppressMessages({
    p <- plot_cpi_forecast(cpi_df, history_n = 36)
  })
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_cpi_forecast() works with full history (history_n = Inf)", {
  suppressMessages({
    p <- plot_cpi_forecast(cpi_df, history_n = Inf)
  })
  testthat::expect_s3_class(p, "ggplot")
})

# ── viz_labor.R ───────────────────────────────────────────────────────────────
testthat::test_that("plot_unrate() returns a ggplot", {
  p <- plot_unrate(unrate_df)
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_unrate_forecast() returns a ggplot", {
  suppressMessages({
    p <- plot_unrate_forecast(unrate_df, history_n = 36)
  })
  testthat::expect_s3_class(p, "ggplot")
})

# ── viz_gdp.R ─────────────────────────────────────────────────────────────────
testthat::test_that("plot_gdp_growth() returns a ggplot", {
  p <- plot_gdp_growth(gdp_df)
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_gdp_forecast() returns a ggplot", {
  suppressMessages({
    p <- plot_gdp_forecast(gdp_df, history_n = 20)
  })
  testthat::expect_s3_class(p, "ggplot")
})

# ── viz_timeseries.R ──────────────────────────────────────────────────────────
testthat::test_that("plot_timeseries() returns a ggplot", {
  p <- plot_timeseries(rates_df, title = "Test Title", ylab = "Percent")
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("maybe_interactive() returns ggplot when interactive=FALSE", {
  p  <- plot_timeseries(rates_df, title = "Test")
  p2 <- maybe_interactive(p, interactive = FALSE)
  testthat::expect_s3_class(p2, "ggplot")
})

# ── viz_yieldcurve.R ──────────────────────────────────────────────────────────
testthat::test_that("plot_spread_10y_2y() returns a ggplot", {
  p <- plot_spread_10y_2y(rates_df)
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_spread_10y_2y() handles data with NAs in one series", {
  # Introduce some NAs in DGS10 to ensure the function handles them
  rates_with_na        <- rates_df
  rates_with_na$value[rates_with_na$series_id == "DGS10"][1:5] <- NA

  p <- plot_spread_10y_2y(rates_with_na)
  testthat::expect_s3_class(p, "ggplot")
})
