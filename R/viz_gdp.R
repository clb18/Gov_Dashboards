# R/viz_gdp.R
#
# Visualization functions for the GDP dashboard.
# All functions accept the standard tibble produced by get_gdp_bundle() and
# return a ggplot object.
#
# Functions:
#   plot_gdp_growth()    → Bar chart of quarterly annualized GDP growth rates
#   plot_gdp_forecast()  → Recent growth bars + 3-quarter ARIMA forecast bands
#
# Usage pattern in a dashboard or presentation:
#   p1 <- plot_gdp_growth(gdp)           # full history bar chart
#   p2 <- plot_gdp_forecast(gdp)         # recent + forecast
#   p1; p2                               # show both

# ── Chart 1: Quarterly GDP Growth (Bar Chart) ─────────────────────────────────

#' Bar chart of annualized real GDP growth by quarter.
#'
#' Bars are colored green for positive quarters and red for contractions,
#' making recessions immediately visible. A horizontal line marks 0%.
#'
#' @param df    Standardized GDP tibble from get_gdp_bundle().
#' @param title Plot title string.
#' @return ggplot object.
plot_gdp_growth <- function(df,
                            title = "Real GDP Growth: Annualized % Change by Quarter") {
  df <- df |>
    dplyr::arrange(date) |>
    dplyr::mutate(direction = ifelse(value >= 0, "Expansion", "Contraction"))

  ggplot(df, aes(x = date, y = value, fill = direction)) +
    geom_col(width = 70, na.rm = TRUE) +   # ~70 day width suits quarterly bars
    geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
    scale_fill_manual(
      values = c("Expansion" = "#4dac26", "Contraction" = "#d01c8b"),
      guide  = guide_legend(title = NULL)
    ) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(
      title    = title,
      subtitle = "Seasonally adjusted annual rate (SAAR)  |  Source: BEA NIPA Table 1.1.1",
      x        = NULL,
      y        = "Annualized % Change"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position    = "bottom",
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# ── Chart 2: Recent GDP Growth + Forecast ────────────────────────────────────

#' Plot recent GDP growth with 3-quarter ARIMA forecast and confidence bands.
#'
#' Shows the last N quarters as bars, then overlays the model forecast as a
#' dashed line with shaded 80% and 95% confidence intervals.
#'
#' @param df          Standardized GDP tibble from get_gdp_bundle().
#' @param history_n   Number of recent quarters to show. Default 20 (5 years).
#' @param title       Plot title string.
#' @return ggplot object.
plot_gdp_forecast <- function(df,
                              history_n = 20,
                              title     = "Real GDP Growth: 3-Quarter Forecast") {
  df_recent <- df |>
    dplyr::arrange(date) |>
    utils::tail(history_n) |>
    dplyr::mutate(direction = ifelse(value >= 0, "Expansion", "Contraction"))

  df_for_fc <- dplyr::select(df_recent, date, value)
  band      <- forecast_next_quarter(df_for_fc)

  ggplot(df_recent, aes(x = date, y = value)) +
    # Historical bars
    geom_col(aes(fill = direction), width = 70, na.rm = TRUE) +
    scale_fill_manual(
      values = c("Expansion" = "#4dac26", "Contraction" = "#d01c8b"),
      guide  = guide_legend(title = NULL)
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
    # 95% forecast band
    geom_ribbon(
      data        = band,
      aes(x = date, ymin = lo95, ymax = hi95),
      inherit.aes = FALSE,
      fill        = "steelblue",
      alpha       = 0.15
    ) +
    # 80% forecast band
    geom_ribbon(
      data        = band,
      aes(x = date, ymin = lo80, ymax = hi80),
      inherit.aes = FALSE,
      fill        = "steelblue",
      alpha       = 0.25
    ) +
    # Forecast mean line
    geom_line(
      data        = band,
      aes(x = date, y = mean),
      inherit.aes = FALSE,
      linewidth   = 0.9,
      linetype    = "dashed",
      color       = "steelblue"
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(
      title    = title,
      subtitle = "Shaded bands: 80% and 95% ARIMA confidence intervals",
      x        = NULL,
      y        = "Annualized % Change"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position    = "bottom",
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    )
}
