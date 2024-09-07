
treeData  <- read.csv("data/TreeData.csv")
animalData <- read.csv("data/AnimalData.csv")
treeAnimalData  <- read.csv("data/TreeAnimalData.csv")

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
  labs(title = "Impact of AQI on Average Animals per Tree",
       x = "AQI Category",
       y = "Average Animals per Tree") +
  theme_minimal() + 
  theme(legend.position = 'none')




