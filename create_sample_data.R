# Sample Unemployment Data
# This mimics the structure of BLS unemployment data

library(tidyverse)
library(lubridate)
install.packages("tibble")
library(tibble)
# Set seed for reproducibility
set.seed(123)

# Generate monthly data from Jan 2020 to Feb 2025
dates <- seq(from = as.Date("2020-01-01"), to = as.Date("2025-02-01"), by = "month")

# Create realistic unemployment rate trajectory
# - Pre-COVID baseline around 3.5%
# - COVID spike to ~14% in April 2020
# - Gradual recovery
# - Recent stabilization around 4%

n_months <- length(dates)

unemployment_rate <- c(
  rep(3.5, 3),  # Jan-Mar 2020: pre-COVID
  c(4.4, 14.7, 13.3, 11.1, 10.2, 8.4, 7.9, 6.9),  # Apr-Nov 2020: COVID impact
  c(6.7, 6.2, 6.0, 5.8, 5.9, 5.5, 5.2, 4.8),  # Dec 2020-Jul 2021: recovery
  c(5.2, 4.7, 4.6, 4.2, 4.2, 3.9, 3.6, 3.7),  # Aug 2021-Mar 2022: continued recovery
  c(3.6, 3.6, 3.5, 3.6, 3.7, 3.5, 3.8, 3.7),  # Apr-Nov 2022: stabilization
  c(3.5, 3.4, 3.6, 3.4, 3.7, 3.6, 3.5, 3.8),  # Dec 2022-Jul 2023
  c(3.8, 3.7, 3.9, 3.8, 3.7, 3.7, 4.1, 4.2),  # Aug 2023-Mar 2024
  c(3.9, 4.0, 4.0, 4.1, 4.2, 4.1, 4.2, 4.1),  # Apr-Nov 2024
  c(4.0, 4.0, 4.1)  # Dec 2024-Feb 2025
)

# Add some noise
unemployment_rate <- unemployment_rate + rnorm(n_months, 0, 0.05)

# National data
national_data <- tibble(
  date = dates,
  unemployment_rate = unemployment_rate,
  labor_force = 160000 + rnorm(n_months, 0, 1000),  # in thousands
  employed = labor_force * (1 - unemployment_rate / 100),
  unemployed = labor_force - employed,
  area = "United States",
  area_type = "National"
)

# Generate state-level data (just a few states for sample)
states <- c("California", "Texas", "Florida", "New York", "Pennsylvania", 
            "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan")

state_data <- map_dfr(states, function(state) {
  # Each state has slightly different baseline and patterns
  state_baseline <- runif(1, 3.0, 5.0)
  state_variation <- runif(1, 0.8, 1.2)
  
  tibble(
    date = dates,
    unemployment_rate = unemployment_rate * state_variation + (state_baseline - 3.5),
    labor_force = runif(1, 5000, 20000) + rnorm(n_months, 0, 100),
    area = state,
    area_type = "State"
  ) %>%
    mutate(
      employed = labor_force * (1 - unemployment_rate / 100),
      unemployed = labor_force - employed
    )
})

# Combine national and state data
unemployment_data <- bind_rows(national_data, state_data) %>%
  mutate(
    year = year(date),
    month = month(date),
    month_name = month(date, label = TRUE, abbr = FALSE)
  ) %>%
  arrange(area, date)

# Calculate month-over-month and year-over-year changes
unemployment_data <- unemployment_data %>%
  group_by(area) %>%
  mutate(
    mom_change = unemployment_rate - lag(unemployment_rate),
    yoy_change = unemployment_rate - lag(unemployment_rate, 12)
  ) %>%
  ungroup()

# Save to RDS for easy loading
saveRDS(unemployment_data, "sample_unemployment_data.rds")

# Also save as CSV for portability
write_csv(unemployment_data, "sample_unemployment_data.csv")

# Preview the data
cat("Sample Unemployment Data Created\n")
cat("================================\n\n")
cat("Date range:", min(unemployment_data$date), "to", max(unemployment_data$date), "\n")
cat("Number of areas:", n_distinct(unemployment_data$area), "\n")
cat("Total observations:", nrow(unemployment_data), "\n\n")

cat("Latest National Unemployment Rate:", 
    unemployment_data %>% 
      filter(area == "United States") %>% 
      slice_tail(n = 1) %>% 
      pull(unemployment_rate) %>% 
      round(2), "%\n\n")

cat("Sample of data:\n")
print(head(unemployment_data %>% select(date, area, unemployment_rate, labor_force), 10))
