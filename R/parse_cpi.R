# R/parse_cpi.R
#
# Fetches and cleans BLS CPI data into the project's standard tibble format.
#
# Primary series:
#   CUSR0000SA0  → CPI-U All Items, Seasonally Adjusted (headline inflation)
#
# Standard output tibble columns:
#   date      <date>  first day of the reference month (e.g., 2024-01-01)
#   value     <dbl>   index level (not YoY %; that is computed in viz_inflation.R)
#   series_id <chr>   BLS series code
#   label     <chr>   human-readable name
#   freq      <chr>   "M" (monthly)
#
# Dependencies: 01_helpers.R, API_bls.R, mock_data.R
#
# BLS API key:  stored in GitHub Secret BLS_API_KEY
#               Register free at https://data.bls.gov/registrationEngine/

# ── BLS response parser ───────────────────────────────────────────────────────

#' Parse a single BLS v2 API series from the raw JSON response.
#'
#' The BLS JSON structure is:
#'   json$Results$series[[i]]$seriesID   → "CUSR0000SA0"
#'   json$Results$series[[i]]$data[[j]]  → list(year, period, periodName, value)
#'   period format: "M01"–"M12" (monthly), "M13" = annual average (excluded)
#'
#' @param json    Raw list returned by bls_timeseries().
#' @param series_id BLS series code string (must match one series in json).
#' @param label   Human-readable label for the series.
#' @param freq    Frequency code; default "M".
#' @return Standardized tibble: date, value, series_id, label, freq.
parse_bls_series <- function(json, series_id, label, freq = "M") {
  series_list <- json$Results$series

  # Find the matching series by ID
  idx <- which(sapply(series_list, function(s) s$seriesID) == series_id)
  if (length(idx) == 0) {
    stop(sprintf("Series '%s' not found in BLS response.", series_id), call. = FALSE)
  }

  raw <- series_list[[idx[1]]]$data

  # Each element of raw is a list; extract fields with sapply
  tibble::tibble(
    year   = as.integer(sapply(raw, `[[`, "year")),
    period = sapply(raw, `[[`, "period"),
    value  = suppressWarnings(as.numeric(sapply(raw, `[[`, "value")))
  ) |>
    # M13 = annual average; exclude it so every row maps to a real month
    dplyr::filter(grepl("^M(0[1-9]|1[0-2])$", period)) |>
    dplyr::mutate(
      month     = as.integer(substr(period, 2, 3)),
      date      = as.Date(sprintf("%d-%02d-01", year, month)),
      series_id = series_id,
      label     = label,
      freq      = freq
    ) |>
    dplyr::select(date, value, series_id, label, freq) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::arrange(date)
}

# ── Main bundle function ──────────────────────────────────────────────────────

#' Fetch and clean CPI-U data.
#'
#' @param start_year  Integer. First year of data to request. Default 2000.
#' @param end_year    Integer. Last year of data to request. Default current year.
#' @param use_mock    Logical. TRUE → synthetic data (no API key needed).
#'   When BLS_API_KEY is added to GitHub Secrets, set to:
#'     USE_MOCK <- Sys.getenv("BLS_API_KEY") == ""
#' @param use_cache   Logical. TRUE → read from local CSV cache if available.
#' @param cache       Logical. TRUE → write fetched data to local CSV cache.
#' @return Standardized tibble: date, value, series_id, label, freq.
get_cpi_bundle <- function(start_year = 2000,
                           end_year   = as.integer(format(Sys.Date(), "%Y")),
                           use_mock   = TRUE,
                           use_cache  = TRUE,
                           cache      = TRUE) {

  # Short-circuit: return mock data when flagged or key unavailable
  if (use_mock) {
    message("get_cpi_bundle(): using mock data (USE_MOCK = TRUE)")
    return(mock_cpi(start_date = sprintf("%d-01-01", start_year)))
  }

  # Check cache first
  cache_path <- "data/cache/bls_cpi.csv"
  cached     <- maybe_cache_read(cache_path, use_cache = use_cache)
  if (!is.null(cached)) {
    message("get_cpi_bundle(): loaded from cache")
    return(cached)
  }

  # Live API call
  message("get_cpi_bundle(): fetching from BLS API")
  series <- "CUSR0000SA0"
  json   <- bls_timeseries(series, start_year, end_year)
  df     <- parse_bls_series(json, series, "CPI-U (All Items, SA)", freq = "M")

  maybe_cache_write(df, cache_path, cache = cache)
  df
}
