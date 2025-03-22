library(readxl)
library(tseries)
library(stargazer)

# Load data
data <- read_excel("data/VAR_US_DATA.xlsx")

# Select variables
vars <- c("debt", "m3")

# Create empty data frame
results <- data.frame(
  Variable = character(),
  KPSS_Level = numeric(),
  KPSS_Diff1 = numeric(),
  KPSS_Diff2 = numeric(),
  Final_KPSS_Order = integer(),
  stringsAsFactors = FALSE
)

# Loop through variables
for (var in vars) {
  series <- data[[var]]
  
  p0 <- kpss.test(series)$p.value
  p1 <- kpss.test(diff(series))$p.value
  p2 <- kpss.test(diff(diff(series)))$p.value
  
  order <- if (p0 >= 0.05) {
    0
  } else if (p1 >= 0.05) {
    1
  } else if (p2 >= 0.05) {
    2
  } else {
    NA
  }
  
  results <- rbind(results, data.frame(
    Variable = var,
    KPSS_Level = round(p0, 4),
    KPSS_Diff1 = round(p1, 4),
    KPSS_Diff2 = round(p2, 4),
    Final_KPSS_Order = order
  ))
}

# Save as CSV
write.csv(results, "results/kpss_results.csv", row.names = FALSE)

# Save as LaTeX
latex_out <- stargazer(results, type = "latex", summary = FALSE,
                       title = "KPSS Test Results for Debt and M3")
writeLines(latex_out, "results/kpss_results.tex")

# Also print the results
print(results)
