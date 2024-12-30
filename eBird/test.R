# Load required libraries
library(auk)
library(dplyr)
library(lubridate)

# Define the file paths for the datasets
files <- c(
  "./eBird/vultureData1/ebd_IN_cinvul1_202101_202409_smp_relJul-2024.txt",
  "./eBird/vultureData1/ebd_IN_egyvul1_202101_202409_smp_relJul-2024.txt",
  "./eBird/vultureData1/ebd_IN_eurgri1_202101_202409_smp_relJul-2024.txt",
  "./eBird/vultureData1/ebd_IN_himgri1_202101_202409_smp_relJul-2024.txt",
  "./eBird/vultureData1/ebd_IN_indvul1_202101_202409_unv_smp_relJul-2024.txt",
  "./eBird/vultureData1/ebd_IN_lammer1_202101_202409_smp_relJul-2024.txt",
  "./eBird/vultureData1/ebd_IN_rehvul1_202101_202409_smp_relJul-2024.txt",
  "./eBird/vultureData1/ebd_IN_slbvul1_202101_202409_smp_relJul-2024.txt",
  "./eBird/vultureData1/ebd_IN_whrvul1_202101_202409_smp_relJul-2024.txt"
)

# Define date ranges for filtering
date_ranges <- list(
  c("2021-09-04", "2021-11-30"),
  c("2022-09-03", "2022-10-02"),
  c("2023-09-01", "2023-09-30"),
  c("2024-09-07", "2024-10-06")
)

# Initialize an empty data frame to store the combined results
combined_data <- data.frame()

# Function to filter the eBird data using auk
filter_ebird_data <- function(file, date_ranges) {
  # Load the eBird data
  ebd_data <- auk_ebd(file)
  
  # Create a filter for each date range and combine them
  for (date_range in date_ranges) {
    ebd_filtered <- ebd_data %>%
      auk_date(date_range) %>%      # Filter by date range
      auk_country("India")          # Filter by country (India)
    
    # Run the filtering process (specifying the output file)
    auk_output_file <- paste0("filtered_", basename(file))
    auk_filter(ebd_filtered, file = auk_output_file, overwrite = TRUE)
    
    # Read the filtered eBird data back into R
    filtered_data <- read_ebd(auk_output_file)
    
    # Select relevant columns
    filtered_data_selected <- filtered_data %>%
      select(common_name, 
             scientific_name, 
             observation_count, 
             state, 
             state_code, 
             county, 
             county_code, 
             locality, 
             locality_id, 
             locality_type, 
             latitude, 
             longitude, 
             observation_date, 
             species_comments)
    
    # Append the filtered data to the combined dataset
    combined_data <<- bind_rows(combined_data, filtered_data_selected)
  }
}

# Loop through each file and apply the filter function
for (file in files) {
  filter_ebird_data(file, date_ranges)
}

# Write the combined filtered data to a CSV file
write.csv(combined_data, "filtered_combined_data.csv", row.names = FALSE)

# Display the first few rows of the combined dataset
head(combined_data)
