# 1. ####

# Load necessary libraries
library(ggplot2)

# Data
tree_data <- data.frame(
  Trees_Nativeness = c("Native Indian Trees", 
                       "Native Trees Suitable for Telangana", 
                       "Naturalized Introduced Trees", 
                       "Introduced Trees"),
  Avg_Animals_Per_Tree = c(15.5, 7.2, 2.4, 5.2)
)

# Create the horizontal bar chart
ggplot(tree_data, aes(x = Avg_Animals_Per_Tree, 
                      y = reorder(Trees_Nativeness, Avg_Animals_Per_Tree))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Avg_Animals_Per_Tree), 
            hjust = -0.2,  # Position the labels to the left of the bars
            size = 3,      # Font size
            color = "black") +
  labs(x = "Average Animals Per Tree", 
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank())  # Remove horizontal minor grid lines


# Create the horizontal bar chart
ggplot(tree_data, aes(x = Avg_Animals_Per_Tree, 
                      y = reorder(Trees_Nativeness, Avg_Animals_Per_Tree))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Avg_Animals_Per_Tree), 
            hjust = -0.1,  # Position the labels to the left of the bars
            size = 3,      # Font size
            color = "black") +
  labs(x = "Average Animals Per Tree", 
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank(),  # Remove horizontal minor grid lines
        axis.title.x = element_text(margin = margin(t = 10)))  # Add top margin to x-axis title




# 2 ####
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # Ensure tidyr is loaded for pivot_longer

# Data preparation
data <- data.frame(
  Tree_Type = c("Avenue Trees", "Non-Avenue Trees"),
  Percentage_Trees = c(52.32, 41.04),
  Percentage_Animals = c(70.39, 26.99),
  Avg_Animals_Per_Tree = c(10.8, 5.3)
)

# Reshape data for plotting (for bar charts)
data_long <- data %>%
  pivot_longer(cols = c(Percentage_Trees, Percentage_Animals), names_to = "Metric", values_to = "Value")

# Create the bar and line chart
ggplot(data_long, aes(x = Tree_Type, fill = Metric)) +
  geom_bar(aes(y = Value), position = "dodge", stat = "identity") +
  geom_line(data = data, aes(x = Tree_Type, y = Avg_Animals_Per_Tree * 10, group = 1), color = "black", size = 1, linetype = "dashed") +  # Adjust scale for line
  geom_point(data = data, aes(x = Tree_Type, y = Avg_Animals_Per_Tree * 10), color = "black", size = 3) +  # Points for line
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Average Animals Per Tree")) +  # Secondary axis for average animals per tree
  labs(title = "Tree Types: Animal Support Insights", 
       y = "Percentage of Total (%)", 
       x = "Tree Type",
       fill = "Metric") +
  scale_fill_manual(values = c("Percentage_Trees" = "skyblue", "Percentage_Animals" = "salmon")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y.right = element_text(color = "black")) +
  geom_text(aes(y = Value + 2, label = round(Value, 1)), position = position_dodge(0.9), size = 4)  # Adding data labels






# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # Ensure tidyr is loaded for pivot_longer

# Data preparation
data <- data.frame(
  Tree_Type = c("Avenue Trees", "Non-Avenue Trees"),
  Percentage_Trees = c(52.32, 41.04),
  Percentage_Animals = c(70.39, 26.99),
  Avg_Animals_Per_Tree = c(10.8, 5.3)
)

# Reshape data for plotting (for bar charts)
data_long <- data %>%
  pivot_longer(cols = c(Percentage_Trees, Percentage_Animals), names_to = "Metric", values_to = "Value")

# Create the bar and line chart
ggplot(data_long, aes(x = Tree_Type, fill = Metric)) +
  geom_bar(aes(y = Value), position = "dodge", stat = "identity") +
  geom_line(data = data, aes(x = Tree_Type, y = Avg_Animals_Per_Tree * 10, group = 1), color = "black", size = 1, linetype = "dashed") +  # Adjust scale for line
  geom_point(data = data, aes(x = Tree_Type, y = Avg_Animals_Per_Tree * 10), color = "black", size = 3) +  # Points for line
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Average Animals Per Tree")) +  # Secondary axis for average animals per tree
  labs(title = "Tree Types: Animal Support Insights", 
       y = "Percentage of Total (%)", 
       x = "Tree Type",
       fill = "Metric") +
  scale_fill_manual(values = c("Percentage_Trees" = "skyblue", "Percentage_Animals" = "salmon")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y.right = element_text(color = "black")) +
  geom_text(aes(y = Value + 2, label = round(Value, 1)), position = position_dodge(0.9), size = 4)  # Adding data labels

