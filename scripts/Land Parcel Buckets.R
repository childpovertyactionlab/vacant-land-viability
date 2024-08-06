# load libraries 
library(dplyr)
library(sf)
library(readr)
library(stringr)

#load dataset

features_added <- st_read("Data/Processed Data/Processed Vacant Land Parcels with Features.geojson")

# Function to categorize parcels
categorize_parcel <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      features_count = sum(c_across(c(School_Flag, Grocery_Flag, Transit_Flag, Clinic_Flag, Park_Flag, 
                                      Rec_Center_Flag, Library_Flag)), na.rm = TRUE),
      tier = case_when(
        ENV_HAZ_Flag ~ "Low",
        features_count >= 5 ~ "High",
        features_count >= 1 & features_count <= 4 ~ "Moderate",
        TRUE ~ "Low"
      )
    ) %>%
    ungroup()
}

# Apply categorization function
features_final <- categorize_parcel(features_added)

# remove unnecessary columns
features_final <- features_final %>%
  select(-Rec_Center_Flag1, -Rec_Center_Flag2, -Bus_Stop_Flag, -Rail_Station_Flag, -features_count, -MULTIPLE_ACCOUNTS, -BLDG_ID, -UNIT_ID)

st_write(features_final, "Data/Processed Data/Processed Vacant Land Parcel with Features and Buckets.geojson", 
         delete_dsn = TRUE)

