# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vars)
library(scales)

# Load your data
df <- read_excel("data/VAR_US_DATA.xlsx")
df$year <- as.numeric(df$year)

# Prepare differenced data
df_diff <- df %>%
  arrange(year) %>%
  mutate(across(-year, ~ c(NA, diff(.)), .names = "d_{.col}")) %>%
  drop_na()

# Select only differenced variables for VAR
df_diff_vars <- dplyr::select(df_diff, starts_with("d_"))

# Estimate VAR(1)
var_model <- VAR(df_diff_vars, p = 1, type = "const")

# Extract fitted and residuals
fitted_vals <- as.data.frame(fitted(var_model))
residual_vals <- as.data.frame(residuals(var_model))
actual_vals <- df_diff_vars[-1, ]  # aligned to match dimensions
years_aligned <- df_diff$year[-1]

# Normalize function
normalize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Combine and pivot longer
to_long <- function(mat, label) {
  mat %>%
    mutate(year = years_aligned) %>%
    pivot_longer(-year, names_to = "Variable", values_to = "Value") %>%
    mutate(Value = normalize(Value),
           Type = label)
}

long_actual   <- to_long(actual_vals,   "Actual")
long_fitted   <- to_long(fitted_vals,   "Fitted")
long_residual <- to_long(residual_vals, "Residual")

# Function to plot all variables together in one plot
plot_combined <- function(data, title) {
  ggplot(data, aes(x = year, y = Value, color = Variable)) +
    geom_line(linewidth = 1)+
    labs(
      title = title,
      x = "Year",
      y = "Normalized Value",
      color = "Variable"
    ) +
    theme_minimal()
}

# Create and save plots
p1 <- plot_combined(long_actual, "Actual Values of All Variables (Normalized)")
p2 <- plot_combined(long_fitted, "Fitted Values from VAR(1) Model (Normalized)")
p3 <- plot_combined(long_residual, "Residuals from VAR(1) Model (Normalized)")

ggsave("results/var1_actual_all.png", p1, width = 10, height = 6)
ggsave("results/var1_fitted_all.png", p2, width = 10, height = 6)
ggsave("results/var1_residuals_all.png", p3, width = 10, height = 6)
