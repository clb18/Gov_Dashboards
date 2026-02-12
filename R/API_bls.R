bls_timeseries <- function(series_ids,
                           start_year,
                           end_year,
                           api_key = Sys.getenv("BLS_API_KEY", unset = "")) {
  
  if (api_key == "") stop("BLS_API_KEY not set.", call. = FALSE)
  
  url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
  
  body <- list(
    seriesid = as.list(series_ids),
    startyear = as.character(start_year),
    endyear = as.character(end_year),
    registrationkey = api_key
  )
  
  resp <- request(url) |>
    req_method("POST") |>
    req_body_json(body) |>
    req_perform()
  
  json <- resp |> resp_body_json()
  
  # Return raw JSON; parsing will be handled in parse-cpi / parse-unrate
  json
}
