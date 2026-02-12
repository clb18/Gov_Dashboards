forecast_next_quarter <- function(df_one_series) {
  # Requires: date, value
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Install 'forecast' to use forecast bands: install.packages('forecast')", call. = FALSE)
  }
  
  df_one_series <- df_one_series |> arrange(date)
  
  # Downsample to monthly average (works well for daily rates and most monthly series)
  df_m <- df_one_series |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month) |>
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    rename(date = month)
  
  ts_obj <- stats::ts(df_m$value, frequency = 12)
  fit <- forecast::auto.arima(ts_obj)
  fc  <- forecast::forecast(fit, h = 3, level = c(80, 95))
  
  last_date <- max(df_m$date, na.rm = TRUE)
  future_dates <- seq(from = floor_date(last_date, "month") %m+% months(1),
                      by = "1 month", length.out = 3)
  
  tibble::tibble(
    date = future_dates,
    mean = as.numeric(fc$mean),
    lo80 = as.numeric(fc$lower[,1]),
    hi80 = as.numeric(fc$upper[,1]),
    lo95 = as.numeric(fc$lower[,2]),
    hi95 = as.numeric(fc$upper[,2])
  )
}
