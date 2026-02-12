plot_timeseries <- function(df, title, ylab = NULL) {
  ggplot(df, aes(x = date, y = value, color = label)) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    labs(title = title, x = NULL, y = ylab, color = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

maybe_interactive <- function(p, interactive = FALSE) {
  if (!interactive) return(p)
  if (!requireNamespace("plotly", quietly = TRUE)) return(p)
  plotly::ggplotly(p)
}
