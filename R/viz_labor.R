# R/viz_labor.R
#
# Visualization functions for the Labor Market dashboard.
# All functions accept the standard tibble produced by get_labor_bundle() and
# return a ggplot object.
#
# Functions:
#   plot_unrate()          → Unemployment rate over time with recession shading
#   plot_unrate_forecast() → Recent unemployment + 3-month ARIMA forecast bands
#
# Usage pattern in a dashboard or presentation:
#   p1 <- plot_unrate(labor)           # full history
#   p2 <- plot_unrate_forecast(labor)  # recent + forecast
#   p1; p2                             # show both

# ── NBER Recession bands (approximate) ───────────────────────────────────────
# These date ranges mark NBER-designated US recessions.
# They are used in plot_unrate() to shade the background.
# Update as needed when new recessions are designated.

.nber_recessions <- tibble::tribble(
  ~start,          ~end,
  "1990-07-01",    "1991-03-01",   # Early 90s
  "2001-03-01",    "2001-11-01",   # Dot-com / 9-11
  "2007-12-01",    "2009-06-01",   # Great Financial Crisis
  "2020-02-01",    "2020-04-01"    # COVID-19
) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.Date))

# ── Chart 1: Unemployment Rate (Full History) ─────────────────────────────────

#' Plot the U-3 unemployment rate over time with NBER recession shading.
#'
#' Grey vertical bands mark NBER recession periods for economic context.
#'
#' @param df    Standardized unemployment tibble from get_labor_bundle().
#' @param title Plot title string.
#' @return ggplot object.
plot_unrate <- function(df,
                        title = "U.S. Unemployment Rate (U-3, Seasonally Adjusted)") {

  # Clip recession bands to the date range of the data
  date_min <- min(df$date, na.rm = TRUE)
  date_max <- max(df$date, na.rm = TRUE)
  recessions <- .nber_recessions |>
    dplyr::filter(end >= date_min & start <= date_max) |>
    dplyr::mutate(
      start = pmax(start, date_min),
      end   = pmin(end,   date_max)
    )

  ggplot(df, aes(x = date, y = value)) +
    # Recession shading (drawn first so lines sit on top)
    geom_rect(
      data        = recessions,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill        = "grey80",
      alpha       = 0.5
    ) +
    geom_line(linewidth = 0.7, color = "#377eb8", na.rm = TRUE) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(
      labels = scales::label_number(suffix = "%"),
      limits = c(0, NA)
    ) +
    labs(
      title    = title,
      subtitle = "Grey bands = NBER recession periods",
      x        = NULL,
      y        = "Unemployment Rate (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

# ── Chart 2: Unemployment Rate + Forecast ────────────────────────────────────

#' Plot recent unemployment rate with 3-month ARIMA forecast and confidence bands.
#'
#' @param df          Standardized unemployment tibble from get_labor_bundle().
#' @param history_n   Number of recent months to show. Default 60 (5 years).
#' @param title       Plot title string.
#' @return ggplot object.
plot_unrate_forecast <- function(df,
                                 history_n = 60,
                                 title     = "Unemployment Rate: 3-Month Forecast") {
  df_recent <- utils::tail(dplyr::arrange(df, date), history_n)

  df_for_fc <- dplyr::select(df_recent, date, value)
  band      <- forecast_next_quarter(df_for_fc)

  ggplot(df_recent, aes(x = date, y = value)) +
    geom_line(linewidth = 0.7, color = "#377eb8", na.rm = TRUE) +
    # 95% confidence band
    geom_ribbon(
      data        = band,
      aes(ymin = lo95, ymax = hi95),
      inherit.aes = FALSE,
      fill        = "#377eb8",
      alpha       = 0.12
    ) +
    # 80% confidence band
    geom_ribbon(
      data        = band,
      aes(ymin = lo80, ymax = hi80),
      inherit.aes = FALSE,
      fill        = "#377eb8",
      alpha       = 0.22
    ) +
    # Forecast mean line
    geom_line(
      data        = band,
      aes(y = mean),
      inherit.aes = FALSE,
      linewidth   = 0.8,
      linetype    = "dashed",
      color       = "#377eb8"
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(
      labels = scales::label_number(suffix = "%"),
      limits = c(0, NA)
    ) +
    labs(
      title    = title,
      subtitle = "Shaded bands: 80% and 95% ARIMA confidence intervals",
      x        = NULL,
      y        = "Unemployment Rate (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}
