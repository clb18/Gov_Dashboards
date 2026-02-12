`%||%` <- function(x, y) if (is.null(x) || is.na(x) || x == "") y else x

get_env_or_stop <- function(var) {
  val <- Sys.getenv(var, unset = "")
  if (val == "") stop(sprintf("Missing environment variable: %s", var), call. = FALSE)
  val
}

as_date_safe <- function(x) {
  # FRED dates are "YYYY-MM-DD"
  as.Date(x)
}

maybe_cache_write <- function(df, path, cache = TRUE) {
  if (!cache) return(invisible(FALSE))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(df, path)
  invisible(TRUE)
}

maybe_cache_read <- function(path, use_cache = TRUE) {
  if (!use_cache) return(NULL)
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE)
}
