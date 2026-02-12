plot_spread_10y_2y <- function(rates_df) {
  wide <- rates_df |>
    filter(series_id %in% c("DGS2","DGS10")) |>
    select(date, series_id, value) |>
    pivot_wider(names_from = series_id, values_from = value) |>
    mutate(spread = DGS10 - DGS2) |>
    filter(!is.na(spread))
  
  ggplot(wide, aes(date, spread)) +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_line(linewidth = 0.7) +
    labs(title = "10Y–2Y Treasury Spread", x = NULL, y = "Percentage points") +
    theme_minimal(base_size = 12)
}