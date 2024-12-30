# install.packages("tidyverse")

library(tidyverse)
library(dplyr)

locationData <- read.csv("./HATS/data/LocationData.csv")
treeData  <- read.csv("./HATS/data/TreeData.csv")
treeAnimalData  <- read.csv("./HATS/data/TreeAnimalData.csv")

colnames(locationData)
colnames(treeAnimalData)
colnames(treeData)

# Top 10 tree species #####

tree_counts <- treeData %>%
  group_by(TreeSpeciesName) %>%
  summarize(TreeCount = n()) %>%
  arrange(desc(TreeCount))

identified_trees_counts <- tree_counts %>%
  filter(TreeSpeciesName != "Unidentified / Other")

identified_significant_tree_counts <- tree_counts %>%
  filter(TreeSpeciesName != "Unidentified / Other" & TreeCount > 5)

identified_significant_tree_counts

print(identified_significant_tree_counts, n=Inf)

top_10_identified_significant_tree_counts <- identified_significant_tree_counts %>%
  top_n(10, wt = TreeCount)

ggplot(top_10_identified_significant_tree_counts, aes(x = reorder(TreeSpeciesName, TreeCount), y = TreeCount)) +
  geom_bar(stat = "identity", fill = "#4682b4") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 10 tree species observed",
       x = "",
       y = "") +
  theme_minimal()


# Nativeness Summary ####

# Merge tree counts with nativeness data
nativeness <- identified_significant_tree_counts %>%
  left_join(treeData %>% select(TreeSpeciesName, Nativeness) %>% distinct(), by = "TreeSpeciesName")

# Summarize the data by Nativeness
nativeness_summary <- nativeness %>%
  group_by(Nativeness) %>%
  summarize(
    TotalTreeCount = sum(TreeCount),
    SpeciesCount = n(),
    ExampleSpecies = paste(unique(TreeSpeciesName), collapse = ", ")
  ) %>%
  arrange(desc(TotalTreeCount))

# Print the summarized data nicely
print(nativeness_summary)

# Create a bar chart to display the total tree count by nativeness with species names
ggplot(nativeness_summary, aes(x = reorder(Nativeness, SpeciesCount), y = SpeciesCount, fill = Nativeness)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = ExampleSpecies),nudge_x = -0.2, hjust = 0.1, size = 3.5, color = "black") +  # Add species names
  #coord_flip() +  # Flip coordinates for better readability
  labs(title = "Species by Nativeness",
       x = "Nativeness",
       y = "Species Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") + # Optional: use a color palette for better visualization
  theme(legend.position = "none")  # Remove the legend
