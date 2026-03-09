# tests/run_tests.R
#
# Run the full unit test suite for the Gov Dashboards project.
#
# Usage (from the project root directory in R or RStudio):
#   source("tests/run_tests.R")
#
# Or from the terminal:
#   Rscript tests/run_tests.R
#
# Tests do NOT require API keys — they use mock data and test function
# behavior (correct output format, column names, chart types, etc.).
#
# To add a new test file: create tests/testthat/test-<topic>.R
# and it will be picked up automatically by test_dir().

# Ensure testthat is available
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop(
    "The 'testthat' package is required to run tests.\n",
    "Install it with: install.packages('testthat')",
    call. = FALSE
  )
}

# Source all project R modules so test files can use them
message("Loading project R modules...")
source("R/00_config.R")
source("R/01_helpers.R")
source("R/mock_data.R")
source("R/parse_cpi.R")
source("R/parse_labor.R")
source("R/parse_gdp.R")
source("R/parse_rates.R")
source("R/viz_timeseries.R")
source("R/viz_inflation.R")
source("R/viz_labor.R")
source("R/viz_gdp.R")
source("R/viz_yieldcurve.R")
source("R/forecast_nextq.R")

message("Running tests...\n")
testthat::test_dir("tests/testthat", reporter = "progress")
