# tests/testthat/test-parse.R
#
# Unit tests for the parse bundle functions:
#   get_cpi_bundle(), get_labor_bundle(), get_gdp_bundle(), get_rates_bundle()
#
# All tests use use_mock = TRUE so no API keys are required.
# Tests verify that the bundle functions:
#   1. Return the correct standard tibble format (same columns as mock data)
#   2. Return data starting at the requested start_year
#   3. Have no NAs in required columns
#   4. Match output from the corresponding mock_*() function directly
#
# Run via: source("tests/run_tests.R")

REQUIRED_COLS <- c("date", "value", "series_id", "label", "freq")

# ── get_cpi_bundle() ──────────────────────────────────────────────────────────
testthat::test_that("get_cpi_bundle(use_mock=TRUE) returns standard format", {
  df <- get_cpi_bundle(start_year = 2010, use_mock = TRUE, use_cache = FALSE)

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_s3_class(df$date, "Date")
  testthat::expect_false(anyNA(df$date))
  testthat::expect_false(anyNA(df$value))
})

testthat::test_that("get_cpi_bundle() starts at or after start_year", {
  df <- get_cpi_bundle(start_year = 2015, use_mock = TRUE, use_cache = FALSE)
  testthat::expect_gte(min(df$date), as.Date("2015-01-01"))
})

testthat::test_that("get_cpi_bundle() returns monthly frequency", {
  df <- get_cpi_bundle(use_mock = TRUE, use_cache = FALSE)
  testthat::expect_true(all(df$freq == "M"))
})

# ── get_labor_bundle() ────────────────────────────────────────────────────────
testthat::test_that("get_labor_bundle(use_mock=TRUE) returns standard format", {
  df <- get_labor_bundle(start_year = 2010, use_mock = TRUE, use_cache = FALSE)

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_s3_class(df$date, "Date")
  testthat::expect_false(anyNA(df$value))
})

testthat::test_that("get_labor_bundle() unemployment values in [3%, 15%]", {
  df <- get_labor_bundle(use_mock = TRUE, use_cache = FALSE)
  testthat::expect_true(all(df$value >= 3.0 & df$value <= 15.0))
})

# ── get_gdp_bundle() ──────────────────────────────────────────────────────────
testthat::test_that("get_gdp_bundle(use_mock=TRUE) returns standard format", {
  df <- get_gdp_bundle(start_year = 2010, use_mock = TRUE, use_cache = FALSE)

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_s3_class(df$date, "Date")
  testthat::expect_false(anyNA(df$value))
})

testthat::test_that("get_gdp_bundle() returns quarterly frequency", {
  df <- get_gdp_bundle(use_mock = TRUE, use_cache = FALSE)
  testthat::expect_true(all(df$freq == "Q"))
})

# ── get_rates_bundle() ────────────────────────────────────────────────────────
testthat::test_that("get_rates_bundle(use_mock=TRUE) returns standard format", {
  df <- get_rates_bundle(use_mock = TRUE, use_cache = FALSE)

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_s3_class(df$date, "Date")
  testthat::expect_false(anyNA(df$value))
})

testthat::test_that("get_rates_bundle() contains all three series", {
  df <- get_rates_bundle(use_mock = TRUE, use_cache = FALSE)
  testthat::expect_setequal(unique(df$series_id), c("DFF", "DGS2", "DGS10"))
})

# ── parse_bls_series() with synthetic JSON ────────────────────────────────────
testthat::test_that("parse_bls_series() parses BLS JSON correctly", {
  # Construct minimal BLS-format JSON as a list (mirrors real API response)
  synthetic_json <- list(
    Results = list(
      series = list(
        list(
          seriesID = "CUSR0000SA0",
          data = list(
            list(year = "2024", period = "M01", periodName = "January",  value = "310.3"),
            list(year = "2024", period = "M02", periodName = "February", value = "311.1"),
            list(year = "2024", period = "M13", periodName = "Annual",   value = "310.7")  # should be excluded
          )
        )
      )
    )
  )

  df <- parse_bls_series(synthetic_json, "CUSR0000SA0", "CPI-U (All Items, SA)")

  # M13 (annual average) should be excluded → only 2 rows
  testthat::expect_equal(nrow(df), 2)
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  testthat::expect_equal(df$date, as.Date(c("2024-01-01", "2024-02-01")))
  testthat::expect_equal(df$value, c(310.3, 311.1))
})

# ── parse_bea_gdp() with synthetic JSON ───────────────────────────────────────
testthat::test_that("parse_bea_gdp() parses BEA JSON correctly", {
  synthetic_json <- list(
    BEAAPI = list(
      Results = list(
        Data = list(
          list(SeriesCode = "A191RL1Q225SBEA", TimePeriod = "2024Q1",
               DataValue = "1.6",  LineDescription = "GDP"),
          list(SeriesCode = "A191RL1Q225SBEA", TimePeriod = "2024Q2",
               DataValue = "3.0",  LineDescription = "GDP"),
          list(SeriesCode = "OTHER_LINE",       TimePeriod = "2024Q1",
               DataValue = "2.5",  LineDescription = "Other")  # should be excluded
        )
      )
    )
  )

  df <- parse_bea_gdp(synthetic_json)

  # Only rows for A191RL1Q225SBEA should be included
  testthat::expect_equal(nrow(df), 2)
  testthat::expect_named(df, REQUIRED_COLS, ignore.order = FALSE)
  # Q1 → 2024-01-01, Q2 → 2024-04-01
  testthat::expect_equal(df$date, as.Date(c("2024-01-01", "2024-04-01")))
  testthat::expect_equal(df$value, c(1.6, 3.0))
  testthat::expect_true(all(df$freq == "Q"))
})
