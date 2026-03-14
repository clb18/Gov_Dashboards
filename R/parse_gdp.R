# R/parse_gdp.R
#
# Fetches and cleans BEA GDP data into the project's standard tibble format.
#
# Primary series:
#   A191RL  → Real GDP, Percent Change from Preceding Period (annualized)
#   Source: BEA NIPA Table 1.1.1, Line 1
#   Note: BEA uses short code "A191RL"; the FRED equivalent is "A191RL1Q225SBEA"
#
# Standard output tibble columns:
#   date      <date>  first month of the reference quarter (e.g., 2024-01-01 = Q1)
#   value     <dbl>   annualized % change from preceding quarter
#   series_id <chr>   BEA series code
#   label     <chr>   human-readable name
#   freq      <chr>   "Q" (quarterly)
#
# Dependencies: 01_helpers.R, API_bea.R, mock_data.R
#
# BEA API key: stored in GitHub Secret BEA_API_KEY
#              Register free at https://apps.bea.gov/API/signup/

# ── BEA response parser ───────────────────────────────────────────────────────

#' Parse BEA NIPA GDP JSON response into the standard tibble format.
#'
#' The BEA JSON structure is:
#'   json$BEAAPI$Results$Data[[i]]$TimePeriod  → "2024Q3"
#'   json$BEAAPI$Results$Data[[i]]$DataValue   → "2.8" (string, may contain commas)
#'   json$BEAAPI$Results$Data[[i]]$SeriesCode  → "A191RL1Q225SBEA"
#'
#' TimePeriod format: "YYYYQn" where n is 1–4.
#' We convert "2024Q3" → 2024-07-01 (first month of Q3).
#'
#' @param json Raw list returned by bea_get().
#' @param series_code BEA series code to extract from the Data list.
#' @param label Human-readable label.
#' @return Standardized tibble: date, value, series_id, label, freq = "Q".
parse_bea_gdp <- function(json, series_code = "A191RL",
                          label = "Real GDP Growth (Annualized %)") {

  data_list <- json$BEAAPI$Results$Data

  if (is.null(data_list) || length(data_list) == 0) {
    stop("BEA response contained no Data elements. Check API params.", call. = FALSE)
  }

  # Flatten to a data frame (BEA returns a list of single-row lists)
  raw <- dplyr::bind_rows(lapply(data_list, as.data.frame, stringsAsFactors = FALSE))

  # Filter to the target series (table may contain multiple lines)
  raw <- dplyr::filter(raw, SeriesCode == series_code)

  if (nrow(raw) == 0) {
    stop(sprintf("Series '%s' not found in BEA response.", series_code), call. = FALSE)
  }

  # Parse "YYYYQn" → first month of that quarter
  quarter_to_date <- function(tp) {
    yr <- as.integer(substr(tp, 1, 4))
    q  <- as.integer(substr(tp, 6, 6))
    month <- (q - 1L) * 3L + 1L
    as.Date(sprintf("%d-%02d-01", yr, month))
  }

  raw |>
    dplyr::mutate(
      date      = quarter_to_date(TimePeriod),
      # DataValue may contain commas (e.g., "1,234.5") — strip before coercing
      value     = suppressWarnings(
        as.numeric(gsub(",", "", DataValue))
      ),
      series_id = series_code,
      label     = label,
      freq      = "Q"
    ) |>
    dplyr::select(date, value, series_id, label, freq) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::arrange(date)
}

# ── Main bundle function ──────────────────────────────────────────────────────

#' Fetch and clean Real GDP growth rate data from BEA.
#'
#' Calls NIPA Table 1.1.1 (Percent Change From Preceding Period in Real GDP).
#'
#' @param start_year  Integer. First year to include. Default 2000.
#' @param end_year    Integer. Last year to include. Default current year.
#' @param use_mock    Logical. TRUE → synthetic data (no API key needed).
#'   When BEA_API_KEY is added to GitHub Secrets, set to:
#'     USE_MOCK <- Sys.getenv("BEA_API_KEY") == ""
#' @param use_cache   Logical. TRUE → use local CSV cache if available.
#' @param cache       Logical. TRUE → write fetched data to cache.
#' @return Standardized tibble: date, value, series_id, label, freq = "Q".
get_gdp_bundle <- function(start_year = 2000,
                           end_year   = as.integer(format(Sys.Date(), "%Y")),
                           use_mock   = TRUE,
                           use_cache  = TRUE,
                           cache      = TRUE) {

  if (use_mock) {
    message("get_gdp_bundle(): using mock data (USE_MOCK = TRUE)")
    return(mock_gdp(start_date = sprintf("%d-01-01", start_year)))
  }

  cache_path <- "data/cache/bea_gdp.csv"
  cached     <- maybe_cache_read(cache_path, use_cache = use_cache)
  if (!is.null(cached)) {
    message("get_gdp_bundle(): loaded from cache")
    return(cached)
  }

  message("get_gdp_bundle(): fetching from BEA API")

  # BEA NIPA API parameters for Table 1.1.1
  years_str <- paste(start_year:end_year, collapse = ",")
  params <- list(
    method      = "GetData",
    DataSetName = "NIPA",
    TableName   = "T10101",       # Table 1.1.1 — Percent Change in Real GDP
    Frequency   = "Q",
    Year        = years_str,
    ResultFormat = "JSON"
  )

  json <- bea_get(params)
  df   <- parse_bea_gdp(json)

  maybe_cache_write(df, cache_path, cache = cache)
  df
}
