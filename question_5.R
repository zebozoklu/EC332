# ------------------------------
# Complete R Script for FEVD Analysis
# ------------------------------

# Load required libraries
library(vars)         # For VAR and FEVD
library(tidyverse)    # For data manipulation and plotting (includes dplyr, ggplot2, tidyr)
library(scales)       # For percent formatting

# Step 1: Load Stationary Data
stationary_data <- read_csv("results/stationary_data.csv")

# Step 2: Fit VAR(1) Model
# Ensure the same set of variables and lag specification as in earlier parts
var_model <- VAR(stationary_data, p = 1, type = "const")
summary(var_model)

# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
# Compute FEVD for a forecast horizon of 10 periods
fevd_result <- fevd(var_model, n.ahead = 10)

# Step 4: Convert FEVD results into a tidy long-format DataFrame
# fevd_result is a list where each element corresponds to a response variable.
fevd_df <- map_dfr(names(fevd_result), function(resp_var) {
  # Each fevd_result[[resp_var]] is a matrix with dimensions (n_ahead x number_of_shocks)
  n_steps <- nrow(fevd_result[[resp_var]])
  fevd_mat <- as.data.frame(fevd_result[[resp_var]])
  # Name columns using the variable names from stationary_data (preserving their order)
  colnames(fevd_mat) <- colnames(stationary_data)
  fevd_mat <- fevd_mat %>%
    mutate(Horizon = 1:n_steps, Response = resp_var) %>%
    pivot_longer(cols = -c(Horizon, Response),
                 names_to = "Shock",
                 values_to = "Contribution")
  fevd_mat
})

# View the first few rows of the FEVD data
print(head(fevd_df))

# Optionally, save the FEVD data to CSV
write_csv(fevd_df, "results/variance_decomposition.csv")

# ------------------------------
# Step 5: Visualize the FEVD Results
# ------------------------------

# (A) Stacked Bar Plot at Horizon = 10
fevd_h10 <- fevd_df %>% filter(Horizon == 10)

#Stacked Area Plot Over Horizons for Selected Response Variables
responses_to_plot <- c("d_debt", "d_gov", "d_inflation", "d_it",
                       "d_m3", "d_tax", "growth")
p_area <- fevd_df %>%
  filter(Response %in% responses_to_plot) %>%
  ggplot(aes(x = Horizon, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.8, color = "black") +
  facet_wrap(~ Response, scales = "free_y") +
  labs(title = "FEVD Over Horizons for Selected Variables",
       x = "Forecast Horizon", y = "Variance Share") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
print(p_area)
ggsave("results/fevd_stacked_area.png", p_area, width = 10, height = 8)
