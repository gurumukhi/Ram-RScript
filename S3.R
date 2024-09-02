# Data for Avenue vs. Non-Avenue Trees
avenue_non_avenue_data <- data.frame(
  Type = c("Avenue Trees", "Non-Avenue Trees"),
  PercentageTreeCount = c(52.32, 41.04),
  PercentageAnimals = c(70.39, 26.99),
  AvgAnimalsPerTree = c(10.8, 5.3)
)
library(ggplot2)
library(tidyr)

# Reshape the data for plotting
avenue_non_avenue_long <- avenue_non_avenue_data %>%
  pivot_longer(cols = c(PercentageTreeCount, PercentageAnimals, AvgAnimalsPerTree),
               names_to = "Metric",
               values_to = "Value")

# Plot Avenue vs. Non-Avenue Trees
avenue_non_avenue_plot <- ggplot(avenue_non_avenue_long, aes(x = Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Avenue vs. Non-Avenue Trees",
       x = "Tree Type",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  scale_fill_manual(values = c("PercentageTreeCount" = "#1f77b4", 
                               "PercentageAnimals" = "#ff7f0e",
                               "AvgAnimalsPerTree" = "#2ca02c"),
                    labels = c("Percentage of Tree Count", "Percentage of Animals", "Avg. Animals per Tree"))

print(avenue_non_avenue_plot)
