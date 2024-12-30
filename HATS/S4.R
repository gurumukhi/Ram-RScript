
treeData  <- read.csv("./HATS/data/TreeData.csv")
animalData <- read.csv("./HATS/data/AnimalData.csv")
treeAnimalData  <- read.csv("./HATS/data/TreeAnimalData.csv")

colnames(treeData)
colnames(animalData)
colnames(treeAnimalData)

aqi_summary <- treeAnimalData %>%
  filter(AQI != '-') %>%
  group_by(AQI) %>%
  summarize(
    Total_Trees = n(),
    Total_Animals = sum(AnimalCount, na.rm = TRUE),
    Average_Animals_Per_Tree = mean(AnimalCount, na.rm = TRUE)
  )

aqi_summary


library(ggplot2)

# Plot the summary statistics

ggplot(aqi_summary, aes(x = factor(AQI, levels = rev(unique(AQI))), y = Average_Animals_Per_Tree, fill = AQI)) +
  geom_bar(stat = "identity") +
  labs(title = "", #Impact of AQI on Average Animals per Tree
       x = "AQI Category",
       y = "Average Animals per Tree") +
  theme_minimal() + 
  theme(legend.position = 'none')



# Assuming 'aqi_summary' has a column 'AQI' with corresponding categories
aqi_colors <- c(
  "Poor" = "#ADD8E6",     # Light Blue
  "Moderate" = "#4682b4", # Medium Blue
  "Good" = "#0E4C92"      # Dark Blue
)


ggplot(aqi_summary, aes(x = factor(AQI, levels = rev(unique(AQI))), 
                        y = Average_Animals_Per_Tree, fill = AQI)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = aqi_colors) +  # Apply custom colors
  labs(title = "", # Impact of AQI on Average Animals per Tree
       x = "AQI Category",
       y = "Average Animals per Tree") +
  theme_minimal() + 
  theme(legend.position = 'none')





ggplot(aqi_summary, aes(x = factor(AQI, levels = rev(unique(AQI))), 
                        y = Average_Animals_Per_Tree, fill = AQI)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = aqi_colors) +  # Apply custom colors
  labs(title = "", # Impact of AQI on Average Animals per Tree
       x = "Air Quality Index Category",
       y = "Average Animals per Tree") +
  theme_minimal() + 
  theme(
    legend.position = 'none',
    axis.title.x = element_text(margin = margin(t = 15, r = 15, b = 15, l = 15)),  # Margin for x-axis title
    axis.title.y = element_text(margin = margin(t = 15, r = 15, b = 15, l = 15))   # Margin for y-axis title
  )




