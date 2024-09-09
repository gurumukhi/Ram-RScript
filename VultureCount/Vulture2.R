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
  
  
  return(data_ebird)
}

# Process data for each year
data_2021 <- process_year_data("data/2021-occurrence.txt", 2021, '04-09-2021', '30-11-2021')
data_2022 <- process_year_data("data/2022-occurrence.txt", 2022, '03-09-2022',  '02-10-2022')
data_2023 <- process_year_data("data/2023-occurrence.csv", 2023, '01-09-2023',  '30–09-2023')

data_2021
data_2022
data_2023


# Combine all years into one table
#final_summary <- bind_rows(data_2021, data_2022, data_2023)

final_summary <- bind_rows(data_2021, data_2022, data_2023)

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
  left_join(common_names, by = "scientificName")


final_summary_with_common_names

colnames(final_summary_with_common_names)

write.csv(final_summary_with_common_names, file = "vulture_count_simplified.csv", row.names = FALSE)


