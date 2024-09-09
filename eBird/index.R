library(auk)
# Define the file path for the eBird dataset
ebird_data <- auk_ebd("ebd_relAug-2024.txt", file_sampling = "ebd_sampling_relAug-2024.txt")

# Filter the data by species, country, and date range
ebird_filtered <- ebird_data %>%
  auk_species(c("Species 1", "Species 2", "Species 3", "Species 4", "Species 5")) %>%
  auk_country("IN") %>%
  auk_date("2019-01-01", "2024-12-31") %>%
  auk_complete()

# Run the filtering to get the data
ebird_auk <- auk_filter(ebird_filtered, file = "filtered_ebird_data.txt")
