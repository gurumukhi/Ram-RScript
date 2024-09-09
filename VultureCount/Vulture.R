# Load necessary libraries
library(dplyr)
library(readr)

# Function to process each year's data
process_year_data <- function(file_path, year, startDate, endDate) {
  data <- read_delim(file_path, delim = "\t")

  # Keep only the required columns
  data_filtered <- data %>%
    select(individualCount, eventDate, stateProvince, locality, 
           decimalLatitude, decimalLongitude, scientificName, 
           verbatimScientificName, datasetKey)
  
  # Filter to keep only eBird data (datasetKey = 4fa7b334-ce0d-4e88-aaae-2e0c138d049e)
  data_ebird <- data_filtered %>%
    filter(datasetKey == "4fa7b334-ce0d-4e88-aaae-2e0c138d049e")
  
  # Sum individualCount by scientificName
  summary_table <- data_ebird %>%
    group_by(scientificName) %>%
    summarise(total_individualCount = sum(individualCount, na.rm = TRUE)) %>%
    mutate(year = year)
  
  summary_table
  
  if(year==2021) {
    summary_table <- summary_table %>%
      mutate(total_individualCount = round(as.numeric(total_individualCount)/3))
    
    summary_table
  }
  
  return(summary_table)
}

# Process data for each year
data_2021 <- process_year_data("data/2021-occurrence.txt", 2021, '04-09-2021', '30-11-2021')
data_2022 <- process_year_data("data/2022-occurrence.txt", 2022, '03-09-2022',  '02-10-2022')
data_2023 <- process_year_data("data/2023-occurrence.csv", 2023, '01-09-2023',  '30–09-2023')

data_2021
data_2022
data_2023

data_2021 <- data_2021 %>%
  mutate(count_2021 = total_individualCount) %>%
  select(-year, -total_individualCount)

data_2022 <- data_2022 %>%
  mutate(count_2022 = total_individualCount) %>%
  select(-year, -total_individualCount)

data_2023 <- data_2023 %>%
  mutate(count_2023 = total_individualCount) %>%
  select(-year, -total_individualCount)


data_2021
data_2022
data_2023

# Combine all years into one table
#final_summary <- bind_rows(data_2021, data_2022, data_2023)

final_summary <- data_2021 %>%
  full_join(data_2022, by = "scientificName") %>%
  full_join(data_2023, by = "scientificName")

common_names <- tibble(
  scientificName = c(
    "Aegypius monachus (Linnaeus, 1766)",
    "Gypaetus barbatus (Linnaeus, 1758)",
    "Gyps bengalensis (Gmelin, 1788)",
    "Gyps fulvus (Hablizl, 1783)",
    "Gyps himalayensis Hume, 1869",
    "Gyps indicus (Scopoli, 1786)",
    "Gyps tenuirostris G.R.Gray, 1844",
    "Neophron percnopterus (Linnaeus, 1758)",
    "Sarcogyps calvus (Scopoli, 1786)"
  ),
  commonName = c(
    "Cinereous Vulture",
    "Bearded Vulture",
    "White-rumped Vulture",
    "Griffon Vulture",
    "Himalayan Vulture",
    "Indian Vulture",
    "Slender-billed Vulture",
    "Egyptian Vulture",
    "Red-headed Vulture"
  )
)

# Merge the common names with the final summary table
final_summary_with_common_names <- final_summary %>%
  left_join(common_names, by = "scientificName") %>%
  select(-scientificName, commonName, count_2021, count_2022, count_2023)


final_summary_with_common_names

colnames(final_summary_with_common_names)

final_summary_long <- final_summary_with_common_names %>%
  pivot_longer(cols = starts_with("count_"), names_to = "year", values_to = "count") %>%
  mutate(year = factor(year, levels = c("count_2021", "count_2022", "count_2023"),
                       labels = c("2021", "2022", "2023")))

# Create a bar plot
ggplot(final_summary_long, aes(x = commonName, y = count, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "WWF-India Vulture Count Data (Yearly comparison between species) | Source: GBIF eBird Dataset",
       x = "Common Name",
       y = "Count",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
