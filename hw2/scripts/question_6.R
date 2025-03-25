# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(tibble)
library(knitr)
library(kableExtra)

# Load your data
df <- read_excel("data/VAR_US_DATA.xlsx")

# Ensure year is numeric
df$year <- as.numeric(df$year)

# Select tax and prepare differenced variable
df_diff <- df %>%
  select(year, tax) %>%
  mutate(d_tax = c(NA, diff(tax))) %>%
  drop_na()

# Fit ARIMA(1,0,0) = AR(1) model on differenced tax
ar_model <- Arima(df_diff$d_tax, order = c(1, 0, 0), include.constant = TRUE)

# Forecast 2 steps ahead
fc <- forecast(ar_model, h = 2)

# Reconstruct level forecasts
last_tax <- tail(df$tax, 1)
level_forecast <- cumsum(c(last_tax, fc$mean))[-1]
level_lower <- cumsum(c(last_tax, fc$lower[,2]))[-1]
level_upper <- cumsum(c(last_tax, fc$upper[,2]))[-1]

# Create forecast dataframe
fc_df <- tibble(
  Year = c(2020, 2021),
  Forecast = round(level_forecast, 2),
  `95% CI Lower` = round(level_lower, 2),
  `95% CI Upper` = round(level_upper, 2)
)

# Save to CSV
write.csv(fc_df, "results/d_tax_forecast.csv", row.names = FALSE)

# Save LaTeX table WITHOUT full table environment
latex_table <- kable(fc_df, format = "latex", booktabs = TRUE, linesep = "") %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)

writeLines(latex_table, "results/d_tax_forecast.tex")

# Plot
ggplot() +
  geom_line(data = df, aes(x = year, y = tax), color = "black") +
  geom_point(data = fc_df, aes(x = Year, y = Forecast), color = "blue", size = 2) +
  geom_line(data = fc_df, aes(x = Year, y = Forecast), color = "blue", linetype = "dashed") +
  geom_ribbon(data = fc_df, aes(x = Year, ymin = `95% CI Lower`, ymax = `95% CI Upper`), fill = "blue", alpha = 0.2) +
  labs(
    title = "Forecast of Tax Revenue for 2020 and 2021",
    subtitle = "Based on AR(1) model in differenced form, inverted to levels (95% CI)",
    x = "Year",
    y = "Tax Revenue"
  ) +
  theme_minimal()

ggsave("results/d_tax_forecast_plot.png", width = 8, height = 5)
