# Unemployment Visualization Demo

This package contains sample unemployment data and reusable visualization functions to demonstrate the dashboard design before connecting to real BLS API data.

## Files Included

1. **create_sample_data.R** - Generates realistic sample unemployment data
2. **viz_unemployment.R** - Reusable visualization functions (6 different charts)
3. **unemployment_dashboard_demo.qmd** - Complete Quarto dashboard demonstration
4. **README.md** - This file

## Quick Start

### Step 1: Install Required Packages

```r
# Install required packages if you don't have them
install.packages(c(
  "tidyverse",   # Data manipulation
  "ggplot2",     # Static plots
  "plotly",      # Interactive plots
  "lubridate",   # Date handling
  "scales",      # Axis formatting
  "knitr"        # Tables
))
```

### Step 2: Generate Sample Data

```r
# Run the data generation script
source("create_sample_data.R")
```

This creates two files:
- `sample_unemployment_data.rds` (R data format)
- `sample_unemployment_data.csv` (portable format)

### Step 3: Test Individual Visualizations

```r
# Load the visualization functions
source("viz_unemployment.R")

# Load the sample data
unemployment_data <- readRDS("sample_unemployment_data.rds")

# Try out different visualizations
viz_unemployment_trend(unemployment_data, areas = "United States")
viz_unemployment_by_area(unemployment_data, n_areas = 10)
viz_unemployment_change(unemployment_data)
```

### Step 4: Render the Full Dashboard

In RStudio:
1. Open `unemployment_dashboard_demo.qmd`
2. Click "Render" button
3. HTML dashboard will open in your browser

Or from R console:
```r
quarto::quarto_render("unemployment_dashboard_demo.qmd")
```

Or from terminal:
```bash
quarto render unemployment_dashboard_demo.qmd
```

## Available Visualization Functions

All functions are in `viz_unemployment.R` and are designed to be used independently:

### 1. `viz_unemployment_trend()`
Line chart showing unemployment rate over time.

**Parameters:**
- `data` - unemployment dataframe
- `areas` - character vector of area names (default: "United States")
- `interactive` - TRUE for plotly, FALSE for ggplot (default: TRUE)
- `show_recessions` - shade recession periods (default: FALSE)

**Example:**
```r
viz_unemployment_trend(
  data = unemployment_data,
  areas = c("California", "Texas", "Florida"),
  show_recessions = TRUE
)
```

### 2. `viz_unemployment_by_area()`
Bar chart comparing current unemployment rates across areas.

**Parameters:**
- `data` - unemployment dataframe
- `n_areas` - number of areas to show (default: 10)
- `sort_desc` - sort descending if TRUE (default: TRUE)
- `current_date` - date for comparison (default: most recent)
- `interactive` - TRUE for plotly (default: TRUE)

**Example:**
```r
viz_unemployment_by_area(
  data = unemployment_data,
  n_areas = 15,
  sort_desc = TRUE
)
```

### 3. `viz_unemployment_change()`
Column chart showing month-over-month changes.

**Parameters:**
- `data` - unemployment dataframe
- `area` - area name (default: "United States")
- `n_months` - number of months to show (default: 12)
- `interactive` - TRUE for plotly (default: TRUE)

**Example:**
```r
viz_unemployment_change(
  data = unemployment_data,
  area = "California",
  n_months = 18
)
```

### 4. `viz_unemployment_yoy()`
Bar chart comparing current vs. year-ago unemployment.

**Parameters:**
- `data` - unemployment dataframe
- `areas` - character vector of areas
- `current_date` - date for comparison (default: most recent)
- `interactive` - TRUE for plotly (default: TRUE)

**Example:**
```r
viz_unemployment_yoy(
  data = unemployment_data,
  areas = c("United States", "California", "Texas")
)
```

### 5. `get_unemployment_summary()`
Returns list of key unemployment metrics.

**Parameters:**
- `data` - unemployment dataframe
- `area` - area name (default: "United States")

**Returns:** List with:
- `current_rate` - latest unemployment rate
- `current_date` - date of latest data
- `mom_change` - month-over-month change
- `yoy_change` - year-over-year change
- `unemployed` - number unemployed (thousands)
- `labor_force` - labor force size (thousands)

**Example:**
```r
summary <- get_unemployment_summary(unemployment_data, "California")
cat("Current rate:", summary$current_rate, "%\n")
```

### 6. `viz_unemployment_facets()`
Small multiples showing trends for multiple areas.

**Parameters:**
- `data` - unemployment dataframe
- `areas` - character vector of areas (default: top 9 states)
- `interactive` - FALSE recommended for facets (default: FALSE)

**Example:**
```r
viz_unemployment_facets(
  data = unemployment_data,
  areas = c("California", "Texas", "New York", "Florida")
)
```

## Data Structure

The sample data includes:

**Columns:**
- `date` - Month (first day of month)
- `unemployment_rate` - Unemployment rate (%)
- `labor_force` - Labor force size (thousands)
- `employed` - Number employed (thousands)
- `unemployed` - Number unemployed (thousands)
- `area` - Geographic area name
- `area_type` - "National" or "State"
- `year`, `month`, `month_name` - Date components
- `mom_change` - Month-over-month change (percentage points)
- `yoy_change` - Year-over-year change (percentage points)

**Coverage:**
- Date range: January 2020 - February 2025
- National data: United States
- State data: 10 states (CA, TX, FL, NY, PA, IL, OH, GA, NC, MI)

## Customization

### Custom Theme

The `theme_unemployment()` function provides consistent styling. You can modify it in `viz_unemployment.R`:

```r
theme_unemployment <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      # ... customize other elements
    )
}
```

### Custom Colors

Color palette is defined in `unemployment_colors` list:

```r
unemployment_colors <- list(
  primary = "#2E86AB",      # Change to your preferred blue
  secondary = "#A23B72",    # Change to your preferred purple
  # ... etc.
)
```

## Next Steps

Once you're happy with the visualizations:

1. **Adapt for Real Data** - Modify functions to accept BLS API data format
2. **Add API Connection** - Create `fetch_bls_data()` function
3. **Add Forecasting** - Implement time series forecasting
4. **Replicate Pattern** - Use same structure for inflation, GDP, interest rates
5. **Deploy** - Set up GitHub Pages hosting

## Troubleshooting

**Problem:** Plots look static instead of interactive
**Solution:** Make sure `plotly` package is installed and `interactive = TRUE`

**Problem:** Date axis looks crowded
**Solution:** Adjust `date_breaks` in the visualization function

**Problem:** Colors don't match your preference
**Solution:** Modify `unemployment_colors` list in `viz_unemployment.R`

**Problem:** Dashboard won't render
**Solution:** Make sure Quarto is installed: https://quarto.org/docs/get-started/

## Questions?

These visualizations are designed to be:
- **Modular** - Each function works independently
- **Reusable** - Same functions across all dashboards
- **Flexible** - Easy to customize colors, labels, dates
- **Interactive** - Plotly integration for hover/zoom/pan

Feel free to modify any function to suit your needs!
