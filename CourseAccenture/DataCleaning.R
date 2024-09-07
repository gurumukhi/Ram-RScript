library(readr)
library(tidyverse)
library(dplyr)

# Cleaning Content file ####
#Load data
content_data <- read_csv("./CourseAccenture/data/Content.csv")

#Remove first unnamed column
content_data <- content_data[,-1]
colnames(content_data)
content_data

#Remove empty URL rows
#content_data <- content_data[content_data$URL != "" & !is.na(content_data$URL),]
#content_data <- content_data[content_data$`Content ID` != "" & !is.na(content_data$`Content ID`),]
content_data


# Cleaning Reactions ####
reaction_data <- read_csv("./CourseAccenture/data/Reactions.csv")
reaction_data <- reaction_data[,-1]
reaction_data
colnames(reaction_data)
nrow(reaction_data)

reaction_data <- reaction_data %>%
  filter(if_any(c('Content ID', 'User ID', 'Type'), ~ !is.na(.) & . != ""))

reaction_data <- reaction_data[!is.na(reaction_data$Datetime),]

nrow(reaction_data)

# Cleaning ReactionTypes ####
#Load data
reaction_type_data <- read_csv("./CourseAccenture/data/ReactionTypes.csv")

#Remove first unnamed column
reaction_type_data <- reaction_type_data[,-1]
colnames(reaction_type_data)
reaction_type_data



content_data
reaction_data
reaction_type_data

final_data <- left_join(reaction_data,content_data,"Content ID")
final_data

colnames(final_data)
colnames(reaction_type_data)

reaction_type_data <- reaction_type_data %>% 
  rename(ContentType = Type)

final_data <- final_data %>% rename(ContentType = Type.x)
final_data <- left_join(final_data, reaction_type_data, "ContentType")

colnames(final_data)

res <- final_data %>%
  filter(Score > 0) %>%
  group_by(Category) %>%
  summarise(total = sum(Score, na.rm = F)) %>%
  arrange(total)

res

# LEARN SUBSETTING ########

# Load necessary libraries
library(dplyr)
library(data.table)

# Create a sample data frame
df <- data.frame(A = 1:5, B = letters[1:5], C = 6:10)

# 1. Selecting Specific Columns

# Single Column by Index
df_A <- df[, 1]

# Multiple Columns by Index
df_AB <- df[, c(1, 2)]

# Single Column by Name
df_B <- df[, "B"]

# Multiple Columns by Name
df_AC <- df[, c("A", "C")]

# 2. Selecting Specific Rows

# Single Row by Index
df_row1 <- df[1, ]

# Multiple Rows by Index
df_rows1_3 <- df[c(1, 3), ]

# 3. Selecting Specific Rows and Columns

# By Row and Column Indexes
df_subset1 <- df[1:3, 2:3]

# By Row Index and Column Names
df_subset2 <- df[1:2, c("A", "C")]

# 4. Logical Subsetting

# Using Logical Conditions
df_filtered <- df[df$A > 3, ]

# Using Logical Conditions for Columns
df_filtered_col <- df[df$B %in% c("b", "d"), ]

# 5. Using dplyr for Subsetting

# Selecting Columns
df_selected <- select(df, A, C)

# Filtering Rows
df_filtered_dplyr <- filter(df, A > 3)

# Selecting Specific Rows and Columns
df_subset_dplyr <- df %>%
  filter(A > 3) %>%
  select(A, C)

# 6. Using data.table for Subsetting

# Create a data table
dt <- data.table(A = 1:5, B = letters[1:5])

# Basic Subsetting
dt_subset <- dt[1:3, .(A, B)]

# Print results to check
print("Single Column by Index:")
print(df_A)
print("Multiple Columns by Index:")
print(df_AB)
print("Single Column by Name:")
print(df_B)
print("Multiple Columns by Name:")
print(df_AC)
print("Single Row by Index:")
print(df_row1)
print("Multiple Rows by Index:")
print(df_rows1_3)
print("By Row and Column Indexes:")
print(df_subset1)
print("By Row Index and Column Names:")
print(df_subset2)
print("Using Logical Conditions:")
print(df_filtered)
print("Using Logical Conditions for Columns:")
print(df_filtered_col)
print("Using dplyr - Selecting Columns:")
print(df_selected)
print("Using dplyr - Filtering Rows:")
print(df_filtered_dplyr)
print("Using dplyr - Selecting Specific Rows and Columns:")
print(df_subset_dplyr)
print("Using data.table - Basic Subsetting:")
print(dt_subset)
