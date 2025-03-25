library(readxl)
library(dplyr)
library(vars)

# --- Step 1: Load data and integration orders ---
data <- read_excel("data/VAR_US_DATA.xlsx")
integration_orders <- read.csv("results/order_of_integration.csv")

# --- Step 2: Stationarize the data based on integration order ---
data_ts <- ts(data[ , -1], start = data$year[1], frequency = 1)  # drop year
stationary_data <- data.frame(year = data$year[-1])  # one row dropped for diff

for (var in integration_orders$Variable) {
  order <- integration_orders %>% filter(Variable == var) %>% pull(PP_Integration_Order)
  
  if (order == 0) {
    # Keep at level (drop first row to match diff)
    stationary_data[[var]] <- data_ts[-1, var]
  } else if (order == 1) {
    # First difference
    stationary_data[[paste0("d_", var)]] <- diff(data_ts[ , var])
  }
}

# Drop year column before running VAR
stationary_vars <- stationary_data[, !(names(stationary_data) %in% "year")]


# --- Step 3: Select optimal lag length ---
var_selection <- VARselect(stationary_vars, lag.max = 5, type = "const")

# --- Step 4: Save VAR selection results ---

# Convert VARselect criteria matrix to a data frame with lag column
varsel_df <- as.data.frame(t(var_selection$criteria))  # transpose so lags are rows
varsel_df$Lag <- as.numeric(rownames(varsel_df))
varsel_df <- varsel_df %>% relocate(Lag)

# Save as CSV
write.csv(varsel_df, "results/var_lag_selection.csv", row.names = FALSE)

write.csv(stationary_vars, "results/stationary_data.csv", row.names = FALSE)


# Save as LaTeX
library(stargazer)
latex_out <- stargazer(varsel_df, type = "latex", summary = FALSE,
                       title = "VAR Lag Length Selection Criteria")
writeLines(latex_out, "results/var_lag_selection.tex")

# Optional: print to view
print(varsel_df)



