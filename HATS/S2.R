# install.packages("tidyverse")
# install.packages("patchwork")

library(patchwork)
library(tidyverse)
library(dplyr)

treeData  <- read.csv("data/TreeData.csv")
animalData <- read.csv("data/AnimalData.csv")
treeAnimalData  <- read.csv("data/TreeAnimalData.csv")

colnames(treeAnimalData)
colnames(treeData)

identified_significant_tree_counts <- treeData %>%
  group_by(TreeSpeciesName) %>%
  summarize(TreeCount = n()) %>%
  arrange(desc(TreeCount)) %>%
  filter(TreeSpeciesName != "Unidentified / Other" & TreeCount > 5) 

nativeness_summary_significant <- identified_significant_tree_counts%>%
  left_join(treeData %>% select(TreeSpeciesName, Nativeness) %>% distinct(), by = "TreeSpeciesName") %>%
  group_by(Nativeness) %>%
  summarize(SpeciesCount = n(), AllTreesCount = sum(TreeCount)) %>%
  # Add a total row
  bind_rows(
    tibble(
      Nativeness = "Grand Total",
      SpeciesCount = sum(.$SpeciesCount),
      AllTreesCount = sum(.$AllTreesCount)
    )
  )

nativeness_summary_significant

# Nativeness-Animal Relation ####

# Filter treeAnimalData to include only significant tree species and join with tree counts
animal_summary_table <- treeAnimalData %>%
  filter(TreeSpeciesName %in% identified_significant_tree_counts$TreeSpeciesName) %>%
  group_by(Nativeness) %>%
  summarize(
    SUM_TotalAnimals = sum(AnimalCount, na.rm = TRUE),
  ) %>%
  # Add a total row
  bind_rows(
    tibble(
      Nativeness = "Grand Total",
      SUM_TotalAnimals = sum(.$SUM_TotalAnimals)
    )
  )

animal_summary_table

combined_summary <- nativeness_summary_significant %>%
  left_join(animal_summary_table, by = "Nativeness") %>%
  mutate(
    AVG_AnimalsPerTree = ifelse(AllTreesCount > 0, SUM_TotalAnimals / AllTreesCount, NA) 
  )

# View the combined summary
print(combined_summary)



# Single Chart ####

library(ggplot2)
library(dplyr)
library(tidyr)

# Create a data frame for plotting to handle both TreeCount, TotalAnimals, and AVG_AnimalsPerTree together
plot_data <- combined_summary %>%
  select(Nativeness, AllTreesCount, SUM_TotalAnimals, AVG_AnimalsPerTree) %>%
  pivot_longer(cols = c(AllTreesCount, SUM_TotalAnimals),
               names_to = "Metric",
               values_to = "Value")

# Define the combined plot
combined_plot <- ggplot() +
  # Add bar chart for Tree Count and Total Animals
  geom_bar(data = plot_data %>% filter(Metric %in% c("AllTreesCount", "SUM_TotalAnimals")),
           aes(x = Nativeness, y = Value, fill = Metric),
           stat = "identity", position = "dodge") +
  
  geom_line(data = combined_summary,
            aes(x = Nativeness, y = AVG_AnimalsPerTree * max(plot_data$Value) / max(combined_summary$AVG_AnimalsPerTree), group = 1),
            color = "purple") +
  geom_point(data = combined_summary,
             aes(x = Nativeness, y = AVG_AnimalsPerTree * max(plot_data$Value) / max(combined_summary$AVG_AnimalsPerTree)),
             color = "purple") +
  # Add labels and titles
  labs(title = "Average Animals by Nativeness",
       x = "Nativeness",
       y = "Count",
       fill = "Metric",
       color = "AverageAnimals") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("AllTreesCount" = "#4CAF50", "SUM_TotalAnimals" = "orange"),
                    labels = c("Trees", "Animals")) +
  scale_color_manual(values = c("AverageAnimals" = "purple"),
                     labels = c("AverageAnimals")) +
  
  scale_y_continuous(sec.axis = sec_axis(~ . * max(combined_summary$AVG_AnimalsPerTree) / max(plot_data$Value), name = "AverageAnimals"))

# Display the combined plot
print(combined_plot)









library(ggplot2)
library(dplyr)
library(tidyr)

# Create a data frame for plotting to handle both TreeCount, TotalAnimals, and AVG_AnimalsPerTree together
plot_data <- combined_summary %>%
  select(Nativeness, AllTreesCount, SUM_TotalAnimals, AVG_AnimalsPerTree) %>%
  pivot_longer(cols = c(AllTreesCount, SUM_TotalAnimals),
               names_to = "Metric",
               values_to = "Value")

# Define the combined plot
combined_plot <- ggplot() +
  # Add bar chart for Tree Count and Total Animals
  geom_bar(data = plot_data %>% filter(Metric %in% c("AllTreesCount", "SUM_TotalAnimals")),
           aes(x = Nativeness, y = Value, fill = Metric),
           stat = "identity", position = "dodge") +
  # Add line plot for AverageAnimals
  geom_line(data = combined_summary,
            aes(x = Nativeness, y = AVG_AnimalsPerTree * max(plot_data$Value) / max(combined_summary$AVG_AnimalsPerTree), color = "AverageAnimals"),
            size = 1) +
  geom_point(data = combined_summary,
             aes(x = Nativeness, y = AVG_AnimalsPerTree * max(plot_data$Value) / max(combined_summary$AVG_AnimalsPerTree), color = "AverageAnimals"),
             size = 2) +
  # Add labels and titles
  labs(title = "Trees and Animals by Nativeness",
       x = "Nativeness",
       y = "Count",
       fill = "Metric",
       color = "AverageAnimals") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("AllTreesCount" = "#4CAF50", "SUM_TotalAnimals" = "orange"),
                    labels = c("Trees", "Animals")) +
  scale_color_manual(values = c("AverageAnimals" = "purple"),
                     labels = c("AverageAnimals")) +
  scale_y_continuous(sec.axis = sec_axis(~ . * max(combined_summary$AVG_AnimalsPerTree) / max(plot_data$Value), name = "AverageAnimals"))

# Display the combined plot
#print(combined_plot)



# Separate charts ####

# Create a data frame for plotting to handle both TreeCount and TotalAnimals together
plot_data <- combined_summary %>%
  select(Nativeness, AllTreesCount, SUM_TotalAnimals, AVG_AnimalsPerTree) %>%
  pivot_longer(cols = c(AllTreesCount, SUM_TotalAnimals, AVG_AnimalsPerTree),
               names_to = "Metric",
               values_to = "Value")

# Plot Tree Count and Total Animals using bar chart
tree_animals_plot <- ggplot(plot_data %>% filter(Metric != "AVG_AnimalsPerTree"),
                            aes(x = Nativeness, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tree Count and Total Animals by Nativeness",
       x = "Nativeness",
       y = "Count",
       fill = "Metric") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("AllTreesCount" = "#4CAF50", "SUM_TotalAnimals" = "orange"),
                    labels = c("Trees", "Animals"))  # Custom labels

# Plot AverageAnimals using line plot
avg_animals_plot <- ggplot(plot_data %>% filter(Metric == "AVG_AnimalsPerTree"),
                           aes(x = Nativeness, y = Value, group = 1)) +
  geom_line(color = "purple") +
  geom_point(color = "purple") +
  labs(title = "AverageAnimals by Nativeness",
       x = "Nativeness",
       y = "AverageAnimals") +
  theme_minimal() +
  coord_flip()

# Combine plots
combined_plot <- tree_animals_plot / avg_animals_plot

# Display the combined plot
#print(combined_plot)
