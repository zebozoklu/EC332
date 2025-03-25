# --- Libraries ---
library(readr)      # for reading CSV files
library(vars)       # for VAR estimation and IRFs
library(dplyr)      # for data manipulation
library(tidyr)      # for reshaping data
library(purrr)      # for functional looping
library(tibble)     # for tibbles

# --- Step 1: Load the Stationary Data ---
# Make sure "results/stationary_data.csv" does not include extra columns (e.g., 'year' or row indices)
stationary_data <- read_csv("results/stationary_data.csv")

# --- Step 2: Fit the VAR(1) Model ---
# (Assume all variables are stationary; if not, use your differenced data)
var_model <- VAR(stationary_data, p = 1, type = "const")

# --- Step 3: Set Up IRF Computation ---
# Use a forecast horizon of 10 periods (from period 0 to period 10)
n_ahead <- 10
variables <- colnames(stationary_data)

# --- Step 4: Loop Over All Impulse-Response Pairs ---
# For each combination, compute the IRF and store in a long-format tibble
irf_all <- map_dfr(variables, function(impulse) {
  map_dfr(variables, function(response) {
    # Compute IRF for the given impulse-response pair.
    # We explicitly specify both impulse and response.
    irf_obj <- irf(var_model,
                   impulse = impulse,
                   response = response,
                   n.ahead = n_ahead,
                   boot = TRUE)
    
    # The IRF values for the specified pair are stored under the impulse name.
    # When response is specified, irf_obj$irf[[impulse]] is a matrix with (n_ahead+1) rows.
    # We extract the first (and only) column.
    irf_vals <- as.vector(irf_obj$irf[[impulse]][, 1])
    
    # Define the horizons from 0 to n_ahead (0 being the initial shock)
    horizons <- 0:n_ahead
    
    tibble(
      Impulse = impulse,
      Response = response,
      Horizon = horizons,
      IRF = irf_vals
    )
  })
})

# --- Step 5: Save the Results ---
write.csv(irf_all, "results/irf_all_pairs.csv", row.names = FALSE)

# Preview the output
print(head(irf_all, 10))
