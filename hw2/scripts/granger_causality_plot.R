library(ggplot2)
library(dplyr)

# Load Granger results
granger_df <- read.csv("results/granger_results.csv")

# Format p-values for labeling
plot_df <- granger_df %>%
  mutate(P_Label = sprintf("%.3f", P_Value)) %>%
    mutate(P_Label = ifelse(P_Value < 0.05,
                        paste0("*", sprintf("%.3f", P_Value)),
                        sprintf("%.3f", P_Value)))

plot_df <- plot_df %>% filter(Cause != Affected)


# Plot: Red (low p) → White → Blue (high p)
ggplot(plot_df, aes(x = Affected, y = Cause, fill = P_Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = P_Label), size = 4, color = "black") +
  scale_fill_gradient2(
    low = "#b2182b", mid = "#f4a582", high = "#92c5de",
    midpoint = 0.05, limits = c(0, 1),
    name = "p-value"
  ) +
  labs(
    title = "Granger Causality Heatmap",
    x = "Affected Variable",
    y = "Cause"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )




# Save output
ggsave("results/granger_causality_heatmap.png", width = 7, height = 6)
