bea_get <- function(params, api_key = Sys.getenv("BEA_API_KEY", unset = "")) {
  if (api_key == "") stop("BEA_API_KEY not set.", call. = FALSE)
  
  base <- "https://apps.bea.gov/api/data"
  req <- request(base) |> req_url_query(UserID = api_key)
  
  # params should be a named list, e.g. list(method="GETDATASETLIST")
  for (nm in names(params)) {
    req <- req |> req_url_query(!!nm := params[[nm]])
  }
  
  resp <- req |> req_perform()
  resp |> resp_body_json()
}
