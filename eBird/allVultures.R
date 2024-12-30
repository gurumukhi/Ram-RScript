#install.packages("auk")
library(auk)

ebd_IN_cinvul1_202101_202409_smp_relJul-2024.txt
ebd_IN_egyvul1_202101_202409_smp_relJul-2024.txt
ebd_IN_eurgri1_202101_202409_smp_relJul-2024.txt
ebd_IN_himgri1_202101_202409_smp_relJul-2024.txt
ebd_IN_indvul1_202101_202409_unv_smp_relJul-2024.txt
ebd_IN_lammer1_202101_202409_smp_relJul-2024.txt
ebd_IN_rehvul1_202101_202409_smp_relJul-2024.txt
ebd_IN_slbvul1_202101_202409_smp_relJul-2024.txt
ebd_IN_whrvul1_202101_202409_smp_relJul-2024.txt

# Define the file path for the eBird dataset (EBD) and Sampling Event Data (SED)
ebird_data <- auk_ebd("./eBird/vultureData1/ebd_IN_indvul1_202101_202409_unv_smp_relJul-2024.txt")

# Filter the data by country, and date range
ebird_filtered <- ebird_data %>%
  auk_country("India") %>%                        # Specify the full country name
  auk_date(c("2023-09-01", "2023-09-30"))         # Provide the date range as a vector


# Run the filtering to get the data
# You must specify both the output file for EBD and sampling data
ebird_auk <- auk_filter(ebird_filtered, 
                        file = "./eBird/vultureData1/filtered_ebird_data.txt",          # Output for filtered EBD
                        #sampling_file = "./eBird/vultureData1/filtered_sampling.txt",
                        filter_sampling = FALSE,   # Do not filter sampling data
                        overwrite = T
)   # Output for filtered SED


# Now, after filtering, read the filtered EBD and SED data back into R
filtered_ebird <- read_ebd("./eBird/vultureData1/filtered_ebird_data.txt")
#filtered_sampling <- read_sampling("./eBird/vultureData1/filtered_sampling.txt")
filtered_sampling <- read_sampling("./eBird/vultureData1/ebd_IN_indvul1_202101_202409_unv_smp_relJul-2024_sampling.txt")

# Combine the filtered EBD and SED data for complete data, including non-observations
#complete_data <- auk_complete(filtered_ebird, filtered_sampling)


colnames(filtered_ebird)

# Load the dplyr package

library(dplyr)

# Select the desired columns from the filtered_ebird dataset
filtered_ebird_selected <- filtered_ebird %>%
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

# View the first few rows of the selected dataset
head(filtered_ebird_selected)

# Load the dplyr package
library(dplyr)

# Select the desired columns from the filtered_ebird dataset
filtered_ebird_selected <- filtered_ebird %>%
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

# View the first few rows of the selected dataset
head(filtered_ebird_selected)


# Write the selected dataset to a CSV file
write.csv(filtered_ebird_selected, "./eBird/vultureData1/filtered_ebird_selected.csv", row.names = FALSE)

