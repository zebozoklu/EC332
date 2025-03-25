library(ggplot2)
library(readr)
library(dplyr)

# --- Step 1: Load the IRF CSV ---
irf_data <- read_csv("results/irf_all_pairs.csv")

# Ensure 'Impulse' and 'Response' are treated as factors for faceting
irf_data <- irf_data %>%
  mutate(Impulse = as.factor(Impulse),
         Response = as.factor(Response))

# --- Step 2: Create a faceted IRF plot ---
# We'll create a grid with rows = Response and columns = Impulse
irf_plot <- ggplot(irf_data, aes(x = Horizon, y = IRF)) +
  geom_line(color = "#1b4f72", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  facet_grid(Response ~ Impulse, scales = "free_y") +
  labs(title = "Impulse Response Functions",
       x = "Horizon (Periods)",
       y = "Impulse Response") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16))

# --- Step 3: Display and Save the Plot ---
print(irf_plot)
ggsave("results/irf_faceted_plot.png", irf_plot, width = 12, height = 10)
