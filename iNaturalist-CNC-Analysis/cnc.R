library(ggplot2)
library(readr)
library(dplyr)
library(scales)

data <- read_csv("./iNaturalist-CNC-Analysis/cnc_data.csv")

head(data)

data <- data %>%
  mutate(Year = as.factor(Year),
         `Global Observations` = as.numeric(gsub("[^0-9]", "", `Global Observations`)),
         `Global Species` = as.numeric(gsub("[^0-9]", "", `Global Species`)),
         `Global Participants` = as.numeric(gsub("[^0-9]", "", `Global Participants`)),
         `Cities Involved Globally` = as.numeric(gsub("[^0-9]", "", `Cities Involved Globally`)),
         `India Observations (Official)` = as.numeric(gsub("[^0-9]", "", `India Observations (Official)`)),
         `India Species (Official)` = as.numeric(gsub("[^0-9]", "", `India Species (Official)`)),
         `India Observers (Official)` = as.numeric(gsub("[^0-9]", "", `India Observers (Official)`)),
         `India Cities` = as.numeric(gsub("[^0-9]", "", `India Cities`))
  )


# Global data plot
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = `Global Observations`, color = "Observations"), size = 1.2) +
  geom_line(aes(y = `Global Species`, color = "Species"), size = 1.2) +
  geom_line(aes(y = `Global Participants`, color = "Participants"), size = 1.2) +
  geom_line(aes(y = `Cities Involved Globally`, color = "Cities Involved"), size = 1.2) +
  scale_y_continuous(labels = comma) +
  labs(title = "Global City Nature Challenge Data (2016-2024)",
       x = "Year",
       y = "Count",
       color = "Global Metrics") +
  theme_minimal()

# India data plot
ggplot(data, aes(x = Year)) +
  #geom_line(aes(y = `India Observations (Official)`, color = "Observations", group = 1), size = 1.2) +
  geom_line(aes(y = `India Species (Official)`, color = "Species", group = 1), size = 1.2) +
  geom_line(aes(y = `India Observers (Official)`, color = "Observers", group = 1), size = 1.2) +
  geom_line(aes(y = `India Cities`, color = "Cities Involved", group = 1), size = 1.2) +
  scale_y_continuous(labels = comma) +
  labs(title = "India City Nature Challenge Data (2018-2024)",
       x = "Year",
       y = "Count",
       color = "India Metrics") +
  theme_minimal()

# Calculate India's percentage contribution
data <- data %>%
  mutate(Percent_Cities = (`India Cities` / `Cities Involved Globally`) * 100,
         Percent_Observations = (`India Observations (Official)` / `Global Observations`) * 100)

# Plot the percentage contribution
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = Percent_Cities, color = "India's % of Cities", group = 1), size = 1.2) +
  geom_line(aes(y = Percent_Observations, color = "India's % of Observations", group = 1), size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "India's Contribution to Global CNC (in %)",
       x = "Year",
       y = "Percentage",
       color = "Contribution") +
  theme_minimal()












colnames(data)

# Plot 1: Global Observations Over the Years
ggplot(data, aes(x = Year, y = `Global Observations`, group = 1)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(title = "Global Observations during City Nature Challenge Over the Years",
       x = "Year", y = "Global Observations") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# Plot 2: Global Species Over the Years
ggplot(data, aes(x = Year, y = `Global Species`, group = 1)) +
  geom_line(color = "green", size = 1.2) +
  geom_point(color = "orange", size = 3) +
  labs(title = "Global Species Over the Years",
       x = "Year", y = "Global Species") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# Plot 3: Global Participants Over the Years
ggplot(data, aes(x = Year, y = `Global Participants`, group = 1)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "yellow", size = 3) +
  labs(title = "Global Participants Over the Years",
       x = "Year", y = "Global Participants") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# Plot 4: India Official Observations Over the Years
ggplot(data, aes(x = Year, y = `India Observations (Official)`, group = 1)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "India Official Observations Over the Years",
       x = "Year", y = "India Observations (Official)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# Plot 5: Cities Involved in India Over the Years
ggplot(data, aes(x = Year, y = `India Cities`, group = 1)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "orange", size = 3) +
  labs(title = "India Cities Involved in CNC Over the Years",
       x = "Year", y = "India Cities") +
  scale_y_continuous(labels = comma) +
  theme_minimal()




# Plot for cities comparison
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = `Cities Involved Globally`, color = "Global Cities", group = 1), size = 1.2) +
  geom_line(aes(y = `India Cities`, color = "India Cities", group = 1), size = 1.2) +
  scale_y_continuous(labels = comma) +
  labs(title = "Number of Cities Involved: Global vs India (2016-2024)",
       x = "Year",
       y = "Number of Cities",
       color = "Metric") +
  theme_minimal()


# Plot for observations comparison
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = `Global Observations`, color = "Global Observations", group = 1), size = 1.2) +
  geom_line(aes(y = `India Observations (Official)`, color = "India Observations", group = 1), size = 1.2) +
  scale_y_continuous(labels = comma) +
  labs(title = "Number of Observations: Global vs India (2016-2024)",
       x = "Year",
       y = "Number of Observations",
       color = "Metric") +
  theme_minimal()

# Plot for species comparison
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = `Global Species`, color = "Global Species", group = 1), size = 1.2) +
  geom_line(aes(y = `India Species (Official)`, color = "India Species", group = 1), size = 1.2) +
  scale_y_continuous(labels = comma) +
  labs(title = "Number of Species: Global vs India (2016-2024)",
       x = "Year",
       y = "Number of Species",
       color = "Metric") +
  theme_minimal()

# Plot for participants comparison
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = `Global Participants`, color = "Global Participants", group = 1), size = 1.2) +
  geom_line(aes(y = `India Observers (Official)`, color = "India Participants", group = 1), size = 1.2) +
  scale_y_continuous(labels = comma) +
  labs(title = "Number of Participants: Global vs India (2016-2024)",
       x = "Year",
       y = "Number of Participants",
       color = "Metric") +
  theme_minimal()


