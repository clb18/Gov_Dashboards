# tests/testthat/test-mock_data.R
#
# Unit tests for R/mock_data.R
#
# Verifies that every mock function:
#   1. Returns a tibble (data.frame)
#   2. Has exactly the required columns in the standard format
#   3. Has no NA values in key fields
#   4. Returns data of the expected frequency (monthly vs. quarterly)
#   5. Returns data starting at or after the requested start_date
#   6. Is deterministic (same output each call due to set.seed)
#
# Run via: source("tests/run_tests.R")

# Required columns in the project's standard tibble format
REQUIRED_COLS <- c("date", "value", "series_id", "label", "freq")

# ── mock_cpi() ────────────────────────────────────────────────────────────────
testthat::test_that("mock_cpi() returns correct structure", {
  df <- mock_cpi()

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_s3_class(df$date, "Date")
  testthat::expect_type(df$value, "double")
  testthat::expect_false(anyNA(df$date))
  testthat::expect_false(anyNA(df$value))
})

testthat::test_that("mock_cpi() returns monthly data", {
  df <- mock_cpi(start_date = "2020-01-01")

  # Check freq field
  testthat::expect_true(all(df$freq == "M"))

  # Check that dates are spaced one month apart (28–31 days)
  gaps <- as.numeric(diff(df$date))
  testthat::expect_true(all(gaps >= 28 & gaps <= 31))
})

testthat::test_that("mock_cpi() respects start_date", {
  df <- mock_cpi(start_date = "2010-06-01")
  testthat::expect_gte(min(df$date), as.Date("2010-06-01"))
})

testthat::test_that("mock_cpi() is deterministic", {
  df1 <- mock_cpi()
  df2 <- mock_cpi()
  testthat::expect_identical(df1, df2)
})

testthat::test_that("mock_cpi() values are positive index numbers", {
  df <- mock_cpi()
  testthat::expect_true(all(df$value > 0))
})

# ── mock_unrate() ─────────────────────────────────────────────────────────────
testthat::test_that("mock_unrate() returns correct structure", {
  df <- mock_unrate()

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_s3_class(df$date, "Date")
  testthat::expect_false(anyNA(df$value))
})

testthat::test_that("mock_unrate() values are in plausible range [3%, 15%]", {
  df <- mock_unrate()
  testthat::expect_true(all(df$value >= 3.0 & df$value <= 15.0))
})

testthat::test_that("mock_unrate() returns monthly data", {
  df <- mock_unrate()
  testthat::expect_true(all(df$freq == "M"))
})

# ── mock_gdp() ────────────────────────────────────────────────────────────────
testthat::test_that("mock_gdp() returns correct structure", {
  df <- mock_gdp()

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_s3_class(df$date, "Date")
  testthat::expect_false(anyNA(df$value))
})

testthat::test_that("mock_gdp() returns quarterly data", {
  df <- mock_gdp()
  testthat::expect_true(all(df$freq == "Q"))

  # Dates should be spaced ~90 days apart (quarter boundaries)
  gaps <- as.numeric(diff(df$date))
  testthat::expect_true(all(gaps >= 89 & gaps <= 93))
})

testthat::test_that("mock_gdp() includes COVID collapse quarter", {
  df <- mock_gdp()
  q2_2020 <- df[df$date == as.Date("2020-04-01"), "value", drop = TRUE]
  testthat::expect_true(length(q2_2020) > 0)
  testthat::expect_lt(q2_2020, -10)   # should be very negative
})

# ── mock_rates() ──────────────────────────────────────────────────────────────
testthat::test_that("mock_rates() returns correct structure", {
  df <- mock_rates()

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_s3_class(df$date, "Date")
  testthat::expect_false(anyNA(df$value))
})

testthat::test_that("mock_rates() returns all three series", {
  df <- mock_rates()
  testthat::expect_setequal(unique(df$series_id), c("DFF", "DGS2", "DGS10"))
})

testthat::test_that("mock_rates() values are non-negative", {
  df <- mock_rates()
  testthat::expect_true(all(df$value >= 0))
})

testthat::test_that("mock_rates() all three series have the same dates", {
  df    <- mock_rates()
  dates <- split(df$date, df$series_id)
  testthat::expect_equal(sort(dates[["DFF"]]), sort(dates[["DGS2"]]))
  testthat::expect_equal(sort(dates[["DFF"]]), sort(dates[["DGS10"]]))
})
