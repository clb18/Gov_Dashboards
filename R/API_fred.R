fred_series_observations <- function(series_id,
                                     api_key = Sys.getenv("FRED_API_KEY", unset = ""),
                                     observation_start = NULL,
                                     file_type = "json") {
  if (api_key == "") stop("FRED_API_KEY not set.", call. = FALSE)
  
  base <- "https://api.stlouisfed.org/fred/series/observations"
  
  req <- request(base) |>
    req_url_query(
      series_id = series_id,
      api_key = api_key,
      file_type = file_type
    )
  
  if (!is.null(observation_start)) {
    req <- req |> req_url_query(observation_start = as.character(observation_start))
  }
  
  resp <- req |> req_perform()
  txt  <- resp |> resp_body_string()
  json <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
  
  tibble::tibble(
    series_id = series_id,
    date = as.Date(json$observations$date),
    value = suppressWarnings(as.numeric(json$observations$value))
  )
}
