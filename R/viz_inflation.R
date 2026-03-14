# R/viz_inflation.R
#
# Visualization functions for the Inflation dashboard.
# All functions accept the standard tibble produced by get_cpi_bundle() and
# return a ggplot object that can be displayed alone or wrapped with
# maybe_interactive() for plotly hover.
#
# Functions:
#   plot_cpi_index()    → Raw CPI index level over time
#   plot_cpi_yoy()      → Year-over-year % change (headline inflation rate)
#   plot_cpi_forecast() → Historical YoY + 3-month ARIMA forecast bands
#
# Usage pattern in a dashboard or presentation:
#   p1 <- plot_cpi_index(cpi)           # show one chart
#   p2 <- plot_cpi_yoy(cpi)             # show another
#   p3 <- plot_cpi_forecast(cpi)        # show forecast
#   p1; p2; p3                          # show all three

# ── Chart 1: CPI Index Level ──────────────────────────────────────────────────

#' Plot raw CPI index level over time.
#'
#' Shows the cumulative price level (not the rate of change). Useful for
#' illustrating total price growth across decades.
#'
#' @param df     Standardized CPI tibble from get_cpi_bundle().
#' @param title  Plot title string.
#' @return ggplot object.
plot_cpi_index <- function(df,
                           title = "CPI-U All Items: Price Index Level") {
  ggplot(df, aes(x = date, y = value)) +
    geom_line(linewidth = 0.7, color = "#e41a1c", na.rm = TRUE) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = title,
      subtitle = "Index, 1982-84 = 100  |  Seasonally Adjusted",
      x        = NULL,
      y        = "Index Level"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

# ── Chart 2: Year-over-Year % Change ─────────────────────────────────────────

#' Plot year-over-year CPI inflation rate.
#'
#' Computes % change vs. 12 months prior from the index level data.
#' A horizontal dashed line marks the Fed's 2% inflation target.
#'
#' @param df     Standardized CPI tibble from get_cpi_bundle().
#' @param title  Plot title string.
#' @return ggplot object.
plot_cpi_yoy <- function(df,
                         title = "CPI-U: Year-over-Year Inflation Rate") {
  df_yoy <- df |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      yoy = (value / dplyr::lag(value, 12) - 1) * 100
    ) |>
    dplyr::filter(!is.na(yoy))

  ggplot(df_yoy, aes(x = date, y = yoy)) +
    # Shade above/below the 2% target for quick visual reference
    geom_hline(
      yintercept = 2,
      linetype   = "dashed",
      linewidth  = 0.5,
      color      = "grey50"
    ) +
    geom_line(linewidth = 0.7, color = "#e41a1c", na.rm = TRUE) +
    annotate(
      "text",
      x     = min(df_yoy$date, na.rm = TRUE),
      y     = 2.3,
      label = "Fed 2% target",
      hjust = 0,
      size  = 3,
      color = "grey40"
    ) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(
      title    = title,
      subtitle = "Percent change from same month one year ago",
      x        = NULL,
      y        = "YoY % Change"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

# ── Chart 3: YoY Inflation + Forecast Bands ──────────────────────────────────

#' Plot YoY inflation with 3-month ARIMA forecast and confidence bands.
#'
#' First computes YoY % change, then applies forecast_next_quarter() to the
#' resulting series and overlays 80% and 95% confidence bands.
#'
#' @param df          Standardized CPI tibble from get_cpi_bundle().
#' @param history_n   Number of recent months to show on left side of chart.
#'   Default 60 (5 years). Set to Inf to show full history.
#' @param title       Plot title string.
#' @return ggplot object.
plot_cpi_forecast <- function(df,
                              history_n = 60,
                              title     = "CPI Inflation: 3-Month Forecast") {
  df_yoy <- df |>
    dplyr::arrange(date) |>
    dplyr::mutate(yoy = (value / dplyr::lag(value, 12) - 1) * 100) |>
    dplyr::filter(!is.na(yoy))

  # Optionally trim to recent history for a cleaner look
  if (is.finite(history_n)) {
    df_yoy <- utils::tail(df_yoy, history_n)
  }

  # Rename value column so forecast_next_quarter() can use it
  df_for_fc <- dplyr::select(df_yoy, date, value = yoy)
  band      <- forecast_next_quarter(df_for_fc)

  ggplot(df_yoy, aes(x = date, y = yoy)) +
    # Historical line
    geom_line(linewidth = 0.7, color = "#e41a1c", na.rm = TRUE) +
    # 95% band (light)
    geom_ribbon(
      data        = band,
      aes(x = date, ymin = lo95, ymax = hi95),
      inherit.aes = FALSE,
      fill        = "#e41a1c",
      alpha       = 0.12
    ) +
    # 80% band (darker)
    geom_ribbon(
      data        = band,
      aes(x = date, ymin = lo80, ymax = hi80),
      inherit.aes = FALSE,
      fill        = "#e41a1c",
      alpha       = 0.22
    ) +
    # Forecast mean line
    geom_line(
      data        = band,
      aes(x = date, y = mean),
      inherit.aes = FALSE,
      linewidth   = 0.8,
      linetype    = "dashed",
      color       = "#e41a1c"
    ) +
    geom_hline(yintercept = 2, linetype = "dotted", color = "grey50") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(
      title    = title,
      subtitle = "Shaded bands: 80% and 95% ARIMA confidence intervals",
      x        = NULL,
      y        = "YoY % Change"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}
