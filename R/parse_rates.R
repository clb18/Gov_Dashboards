clean_rates <- function(df) {
  df |>
    filter(!is.na(date), !is.na(value)) |>
    arrange(date) |>
    mutate(
      freq = "D_or_Mixed",
      label = case_when(
        series_id == "DFF" ~ "Effective Fed Funds Rate",
        series_id == "DGS2" ~ "2Y Treasury",
        series_id == "DGS10" ~ "10Y Treasury",
        TRUE ~ series_id
      )
    )
}

get_rates_bundle <- function(observation_start = "1990-01-01",
                             use_cache = TRUE,
                             cache = TRUE) {
  cache_path <- "data/cache/fred_rates.csv"
  cached <- maybe_cache_read(cache_path, use_cache = use_cache)
  if (!is.null(cached)) return(clean_rates(cached))
  
  series <- c("DFF", "DGS2", "DGS10")
  raw <- purrr::map_dfr(series, ~fred_series_observations(.x, observation_start = observation_start))
  
  maybe_cache_write(raw, cache_path, cache = cache)
  clean_rates(raw)
}
