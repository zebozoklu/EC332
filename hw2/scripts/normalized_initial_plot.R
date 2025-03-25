# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the data
df <- read_excel("data/VAR_US_DATA.xlsx")

# Ensure year is numeric
df$year <- as.numeric(df$year)

# Normalize (z-score) each variable except 'year'
df_norm <- df %>%
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  mutate(norm_value = scale(value)) %>%
  ungroup()

# Plot normalized variables
ggplot(df_norm, aes(x = year, y = norm_value, color = variable)) +
  geom_line(size = 1) +
  labs(
    title = "Normalized Time Series of All Variables",
    x = "Year",
    y = "Normalized Value (z-score)",
    color = "Variable"
  ) +
  theme_minimal()

# Save the plot
ggsave("results/all_variables_normalized_plot.png", width = 8, height = 5)
