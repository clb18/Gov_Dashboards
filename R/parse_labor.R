# R/parse_labor.R
#
# Fetches and cleans BLS unemployment data into the project's standard format.
#
# Primary series:
#   LNS14000000  → Unemployment Rate, Seasonally Adjusted (U-3, civilian)
#
# Standard output tibble columns:
#   date      <date>  first day of the reference month
#   value     <dbl>   unemployment rate in percent
#   series_id <chr>   BLS series code
#   label     <chr>   human-readable name
#   freq      <chr>   "M" (monthly)
#
# Dependencies: 01_helpers.R, API_bls.R, mock_data.R, parse_cpi.R
#   (parse_bls_series() is defined in parse_cpi.R and shared here)
#
# BLS API key: stored in GitHub Secret BLS_API_KEY
#              Register free at https://data.bls.gov/registrationEngine/
#
# NOTE: parse_bls_series() is sourced from parse_cpi.R.
#       Always source parse_cpi.R before parse_labor.R.

# ── Main bundle function ──────────────────────────────────────────────────────

#' Fetch and clean U-3 Unemployment Rate data.
#'
#' @param start_year  Integer. First year of data to request. Default 2000.
#' @param end_year    Integer. Last year to request. Default current year.
#' @param use_mock    Logical. TRUE → synthetic data (no API key needed).
#'   When BLS_API_KEY is added to GitHub Secrets, set to:
#'     USE_MOCK <- Sys.getenv("BLS_API_KEY") == ""
#' @param use_cache   Logical. TRUE → use local CSV cache if available.
#' @param cache       Logical. TRUE → write fetched data to cache.
#' @return Standardized tibble: date, value, series_id, label, freq.
get_labor_bundle <- function(start_year = 2000,
                             end_year   = as.integer(format(Sys.Date(), "%Y")),
                             use_mock   = TRUE,
                             use_cache  = TRUE,
                             cache      = TRUE) {

  if (use_mock) {
    message("get_labor_bundle(): using mock data (USE_MOCK = TRUE)")
    return(mock_unrate(start_date = sprintf("%d-01-01", start_year)))
  }

  cache_path <- "data/cache/bls_unrate.csv"
  cached     <- maybe_cache_read(cache_path, use_cache = use_cache)
  if (!is.null(cached)) {
    message("get_labor_bundle(): loaded from cache")
    return(cached)
  }

  message("get_labor_bundle(): fetching from BLS API")
  series <- "LNS14000000"
  json   <- bls_timeseries(series, start_year, end_year)
  df     <- parse_bls_series(json, series, "Unemployment Rate (SA)", freq = "M")

  maybe_cache_write(df, cache_path, cache = cache)
  df
}
