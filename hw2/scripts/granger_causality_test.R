library(lmtest)
library(dplyr)

# Load data (stationary, already differenced)
data <- read.csv("results/stationary_data.csv")
max_lag <- 1  # from VAR selection
variables <- colnames(data)

# Initialize results
granger_results <- data.frame(Cause = character(),
                              Affected = character(),
                              P_Value = numeric(),
                              Decision = character(),
                              stringsAsFactors = FALSE)

# Loop over all ordered pairs (X → Y)
for (cause in variables) {
  for (affected in setdiff(variables, cause)) {
    # Run Granger test: Does `cause` Granger-cause `affected`?
    formula <- as.formula(paste(affected, "~", cause))
    test_result <- grangertest(formula, order = max_lag, data = data)
    p_val <- test_result$`Pr(>F)`[2]  # p-value of the lagged term
    
    granger_results <- rbind(granger_results, data.frame(
      Cause = cause,
      Affected = affected,
      P_Value = round(p_val, 4),
      Decision = ifelse(p_val < 0.05, "Reject H₀", "Fail to reject H₀"),
      stringsAsFactors = FALSE
    ))
  }
}

# Save output
write.csv(granger_results, "results/granger_results.csv", row.names = FALSE)
