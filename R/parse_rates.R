# R/parse_rates.R
#
# Cleans raw FRED rate data and bundles the three key interest rate series:
#   DFF     → Effective Federal Funds Rate (daily → kept as-is)
#   DGS2    → 2-Year Treasury Constant Maturity Rate
#   DGS10   → 10-Year Treasury Constant Maturity Rate
#
# Output tibble columns (standard across all dashboards):
#   date      <date>   first day of observation period
#   value     <dbl>    rate in percent
#   series_id <chr>    FRED series code
#   label     <chr>    human-readable series name
#   freq      <chr>    "D_or_Mixed" (FRED daily series)
#
# Dependencies: 01_helpers.R (maybe_cache_read/write), API_fred.R, mock_data.R

# ── Internal cleaner ─────────────────────────────────────────────────────────

clean_rates <- function(df) {
  df |>
    dplyr::filter(!is.na(date), !is.na(value)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      freq  = "D_or_Mixed",
      label = dplyr::case_when(
        series_id == "DFF"   ~ "Effective Fed Funds Rate",
        series_id == "DGS2"  ~ "2Y Treasury",
        series_id == "DGS10" ~ "10Y Treasury",
        TRUE                 ~ series_id
      )
    )
}

# ── Main bundle function ─────────────────────────────────────────────────────

#' Fetch and clean the three core interest rate series.
#'
#' @param observation_start Start date string "YYYY-MM-DD". Default "1990-01-01".
#' @param use_mock Logical. If TRUE, returns realistic synthetic data instead of
#'   calling the FRED API. Useful for development or when FRED_API_KEY is absent.
#'   Auto-set in each dashboard via: USE_MOCK <- Sys.getenv("FRED_API_KEY") == ""
#' @param use_cache Logical. If TRUE, reads from data/cache/fred_rates.csv when
#'   available (avoids redundant API calls during a render session).
#' @param cache Logical. If TRUE, writes the fetched data to the cache file.
#' @return Tibble with columns: date, value, series_id, label, freq.
get_rates_bundle <- function(observation_start = "1990-01-01",
                             use_mock          = FALSE,
                             use_cache         = TRUE,
                             cache             = TRUE) {

  # Short-circuit: return mock data for development / missing API key
  if (use_mock) {
    message("get_rates_bundle(): using mock data (USE_MOCK = TRUE)")
    return(mock_rates(start_date = observation_start))
  }

  # Check cache before hitting the API
  cache_path <- "data/cache/fred_rates.csv"
  cached     <- maybe_cache_read(cache_path, use_cache = use_cache)
  if (!is.null(cached)) {
    message("get_rates_bundle(): loaded from cache")
    return(clean_rates(cached))
  }

  # Live API call: fetch all three series and row-bind
  series <- c("DFF", "DGS2", "DGS10")
  message("get_rates_bundle(): fetching from FRED API (", length(series), " series)")
  raw <- purrr::map_dfr(
    series,
    ~fred_series_observations(.x, observation_start = observation_start)
  )

  maybe_cache_write(raw, cache_path, cache = cache)
  clean_rates(raw)
}
