# Load required packages
library(readxl)      # Read Excel files
library(tseries)     # ADF & PP tests
library(dplyr)       # Data manipulation
library(gt)          # Beautiful tables
library(stargazer)   # LaTeX tables

# Define input and output directories
input_dir <- "data/"
output_dir <- "results/"

# Load the dataset from input directory
data <- read_excel(file.path(input_dir, "VAR_US_DATA.xlsx"))

# Drop the "year" column (not needed for tests)
var_names <- names(data)[-1]  

# Function to find integration order for ADF test
find_adf_order <- function(series) {
  d <- 0  # Differencing counter
  p_adf <- adf.test(series)$p.value
  
  while (p_adf > 0.05) {  # Continue until p-value < 0.05
    series <- diff(series, differences = 1)
    d <- d + 1
    p_adf <- adf.test(series)$p.value
  }
  return(d)
}

# Function to find integration order for PP test
find_pp_order <- function(series) {
  d <- 0  # Differencing counter
  p_pp <- pp.test(series)$p.value
  
  while (p_pp > 0.05) {  # Continue until p-value < 0.05
    series <- diff(series, differences = 1)
    d <- d + 1
    p_pp <- pp.test(series)$p.value
  }
  return(d)
}

# Apply both functions to all variables
adf_orders <- sapply(data[var_names], find_adf_order)
pp_orders <- sapply(data[var_names], find_pp_order)

# Create a results table
results_df <- data.frame(
  Variable = var_names,
  ADF_Integration_Order = adf_orders,
  PP_Integration_Order = pp_orders
)


# Save as CSV
write.csv(results_df, file.path(output_dir, "order_of_integration.csv"), row.names = FALSE)

# Save as LaTeX (stargazer)
latex_output <- stargazer(results_df, type = "latex",
                          title = "Stationarity Test Results",
                          summary = FALSE)

# Save LaTeX file
latex_file <- file.path(output_dir, "order_of_integration.tex")
writeLines(latex_output, latex_file)
