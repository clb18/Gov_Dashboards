# Unemployment Data Visualization Functions
# ===========================================
# 
# This file contains standalone, reusable visualization functions for unemployment data.
# Each function can be used independently or combined in a dashboard.
#
# Required packages: ggplot2, plotly, scales, dplyr, lubridate

library(ggplot2)
library(plotly)
library(scales)
library(dplyr)
library(lubridate)

# Custom theme for consistent styling across all visualizations
theme_unemployment <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, color = "gray40", margin = margin(b = 20)),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "top",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90")
    )
}

# Color palette
unemployment_colors <- list(
  primary = "#2E86AB",      # Blue
  secondary = "#A23B72",    # Purple
  accent = "#F18F01",       # Orange
  negative = "#C73E1D",     # Red
  positive = "#06A77D",     # Green
  neutral = "#6C757D"       # Gray
)


# 1. TREND OVER TIME
# ===================
# Shows unemployment rate over time for specified area(s)
#
# Parameters:
#   data: dataframe with columns: date, unemployment_rate, area
#   areas: character vector of area names (default: "United States")
#   interactive: logical, return plotly object if TRUE (default: TRUE)
#   show_recessions: logical, shade recession periods if TRUE (default: FALSE)
#
# Returns: ggplot or plotly object

viz_unemployment_trend <- function(data, 
                                   areas = "United States", 
                                   interactive = TRUE,
                                   show_recessions = FALSE) {
  
  # Filter data for selected areas
  plot_data <- data %>%
    filter(area %in% areas) %>%
    arrange(date)
  
  # Base plot
  p <- ggplot(plot_data, aes(x = date, y = unemployment_rate, color = area)) +
    geom_line(linewidth = 1.2) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b\n%Y",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = "Unemployment Rate Over Time",
      subtitle = paste("Historical unemployment trends for", paste(areas, collapse = ", ")),
      x = "Date",
      y = "Unemployment Rate (%)",
      color = "Area",
      caption = "Source: Bureau of Labor Statistics"
    ) +
    theme_unemployment()
  
  # Add recession shading if requested
  if (show_recessions) {
    # COVID recession: Feb 2020 - Apr 2020
    p <- p + annotate("rect", 
                      xmin = as.Date("2020-02-01"), 
                      xmax = as.Date("2020-04-30"),
                      ymin = -Inf, ymax = Inf,
                      alpha = 0.2, fill = "gray50")
  }
  
  # Color scale
  if (length(areas) == 1) {
    p <- p + scale_color_manual(values = unemployment_colors$primary)
  }
  
  # Make interactive if requested
  if (interactive) {
    p <- ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(hovermode = "x unified")
  }
  
  return(p)
}


# 2. GEOGRAPHIC COMPARISON (BAR CHART)
# =====================================
# Compares current unemployment rates across different areas
#
# Parameters:
#   data: dataframe with columns: date, unemployment_rate, area
#   n_areas: number of areas to show (default: 10)
#   sort_desc: logical, sort descending if TRUE (default: TRUE)
#   current_date: date to use for "current" (default: most recent in data)
#
# Returns: ggplot or plotly object

viz_unemployment_by_area <- function(data, 
                                     n_areas = 10, 
                                     sort_desc = TRUE,
                                     current_date = NULL,
                                     interactive = TRUE) {
  
  # Get most recent date if not specified
  if (is.null(current_date)) {
    current_date <- max(data$date)
  }
  
  # Filter to current date and top N areas
  plot_data <- data %>%
    filter(date == current_date, area_type == "State") %>%
    arrange(if (sort_desc) desc(unemployment_rate) else unemployment_rate) %>%
    head(n_areas) %>%
    mutate(area = factor(area, levels = area))  # Fix order
  
  # Create plot
  p <- ggplot(plot_data, aes(x = unemployment_rate, y = reorder(area, unemployment_rate))) +
    geom_col(fill = unemployment_colors$primary, alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", unemployment_rate)), 
              hjust = -0.2, size = 3.5, fontface = "bold") +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = "Current Unemployment Rates by State",
      subtitle = paste("Top", n_areas, "states as of", format(current_date, "%B %Y")),
      x = "Unemployment Rate (%)",
      y = NULL,
      caption = "Source: Bureau of Labor Statistics"
    ) +
    theme_unemployment() +
    theme(panel.grid.major.y = element_blank())
  
  # Make interactive if requested
  if (interactive) {
    p <- ggplotly(p, tooltip = c("x", "y"))
  }
  
  return(p)
}


# 3. MONTH-OVER-MONTH CHANGE
# ===========================
# Shows recent changes in unemployment rate
#
# Parameters:
#   data: dataframe with columns: date, mom_change, area
#   area: area name to plot (default: "United States")
#   n_months: number of recent months to show (default: 12)
#
# Returns: ggplot or plotly object

viz_unemployment_change <- function(data, 
                                   area = "United States", 
                                   n_months = 12,
                                   interactive = TRUE) {
  
  # Get recent data
  plot_data <- data %>%
    filter(area == !!area) %>%
    arrange(desc(date)) %>%
    head(n_months) %>%
    arrange(date) %>%
    mutate(
      change_type = ifelse(mom_change >= 0, "Increase", "Decrease"),
      date_label = format(date, "%b\n%Y")
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = date, y = mom_change, fill = change_type)) +
    geom_col(alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    scale_fill_manual(values = c(
      "Increase" = unemployment_colors$negative,
      "Decrease" = unemployment_colors$positive
    )) +
    scale_y_continuous(
      labels = function(x) paste0(ifelse(x > 0, "+", ""), x, " pp")
    ) +
    scale_x_date(
      date_labels = "%b\n%Y",
      date_breaks = "1 month"
    ) +
    labs(
      title = "Month-over-Month Change in Unemployment Rate",
      subtitle = paste("Recent changes for", area),
      x = "Date",
      y = "Change (percentage points)",
      fill = "Direction",
      caption = "Source: Bureau of Labor Statistics\npp = percentage points"
    ) +
    theme_unemployment()
  
  # Make interactive if requested
  if (interactive) {
    p <- ggplotly(p, tooltip = c("x", "y", "fill"))
  }
  
  return(p)
}


# 4. YEAR-OVER-YEAR COMPARISON
# =============================
# Compares current unemployment to same month last year
#
# Parameters:
#   data: dataframe with columns: date, unemployment_rate, yoy_change, area
#   areas: character vector of area names
#   current_date: date to use for comparison (default: most recent)
#
# Returns: ggplot or plotly object

viz_unemployment_yoy <- function(data, 
                                areas = "United States",
                                current_date = NULL,
                                interactive = TRUE) {
  
  # Get most recent date if not specified
  if (is.null(current_date)) {
    current_date <- max(data$date)
  }
  
  # Calculate year ago date
  year_ago <- current_date %m-% months(12)
  
  # Get data for both periods
  plot_data <- data %>%
    filter(area %in% areas, date %in% c(current_date, year_ago)) %>%
    mutate(period = ifelse(date == current_date, "Current", "Year Ago")) %>%
    select(area, period, unemployment_rate)
  
  # Create plot
  p <- ggplot(plot_data, aes(x = area, y = unemployment_rate, fill = period)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", unemployment_rate)),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c(
      "Current" = unemployment_colors$primary,
      "Year Ago" = unemployment_colors$secondary
    )) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = "Year-over-Year Unemployment Comparison",
      subtitle = paste(format(current_date, "%B %Y"), "vs.", format(year_ago, "%B %Y")),
      x = NULL,
      y = "Unemployment Rate (%)",
      fill = "Period",
      caption = "Source: Bureau of Labor Statistics"
    ) +
    theme_unemployment()
  
  # Make interactive if requested
  if (interactive) {
    p <- ggplotly(p, tooltip = c("x", "y", "fill"))
  }
  
  return(p)
}


# 5. SUMMARY METRICS CARD
# ========================
# Creates a simple summary of key unemployment metrics
#
# Parameters:
#   data: dataframe with unemployment data
#   area: area name (default: "United States")
#
# Returns: list with key metrics

get_unemployment_summary <- function(data, area = "United States") {
  
  # Get latest and previous values
  latest <- data %>%
    filter(area == !!area) %>%
    arrange(desc(date)) %>%
    slice(1)
  
  previous <- data %>%
    filter(area == !!area) %>%
    arrange(desc(date)) %>%
    slice(2)
  
  # Calculate metrics
  summary <- list(
    current_rate = latest$unemployment_rate,
    current_date = latest$date,
    mom_change = latest$unemployment_rate - previous$unemployment_rate,
    yoy_change = latest$yoy_change,
    unemployed = latest$unemployed,
    labor_force = latest$labor_force
  )
  
  return(summary)
}


# 6. MULTI-AREA COMPARISON OVER TIME
# ===================================
# Small multiples showing trends for multiple areas
#
# Parameters:
#   data: dataframe with unemployment data
#   areas: character vector of areas to compare
#   interactive: logical (default: FALSE for faceted plots)
#
# Returns: ggplot object

viz_unemployment_facets <- function(data, 
                                   areas = NULL,
                                   interactive = FALSE) {
  
  # If no areas specified, use top 9 states by latest unemployment
  if (is.null(areas)) {
    latest_date <- max(data$date)
    areas <- data %>%
      filter(date == latest_date, area_type == "State") %>%
      arrange(desc(unemployment_rate)) %>%
      head(9) %>%
      pull(area)
  }
  
  # Filter data
  plot_data <- data %>%
    filter(area %in% areas)
  
  # Create faceted plot
  p <- ggplot(plot_data, aes(x = date, y = unemployment_rate)) +
    geom_line(color = unemployment_colors$primary, linewidth = 0.8) +
    facet_wrap(~ area, ncol = 3, scales = "free_y") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = "Unemployment Rate Trends by State",
      subtitle = "Historical comparison across selected states",
      x = "Date",
      y = "Unemployment Rate (%)",
      caption = "Source: Bureau of Labor Statistics"
    ) +
    theme_unemployment() +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "gray95", color = NA)
    )
  
  if (interactive) {
    p <- ggplotly(p)
  }
  
  return(p)
}


# EXAMPLE USAGE
# =============
# 
# # Load data
# unemployment_data <- readRDS("sample_unemployment_data.rds")
# 
# # Create trend plot
# viz_unemployment_trend(unemployment_data, areas = "United States")
# 
# # Compare states
# viz_unemployment_by_area(unemployment_data, n_areas = 10)
# 
# # Show recent changes
# viz_unemployment_change(unemployment_data, area = "United States")
# 
# # Year-over-year comparison
# viz_unemployment_yoy(unemployment_data, areas = c("California", "Texas", "Florida"))
# 
# # Get summary metrics
# summary <- get_unemployment_summary(unemployment_data)
# cat("Current unemployment rate:", summary$current_rate, "%\n")
# 
# # Multi-state comparison
# viz_unemployment_facets(unemployment_data, areas = c("California", "Texas", "New York"))
