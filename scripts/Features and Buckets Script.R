## load packages

library(dplyr)
library(readr)
library(sf)
library(stringr)

## set working directory
setwd("C:/Users/theny/Documents/CPAL Summer 2024/Vacant Land Project")

## load datasets

vacant_geo <- st_read("Data/Processed Data/Processed Vacant Land Parcels.geojson")
Dallas_Parks <- st_read("Data/Dallas Parks.geojson")%>%
  st_transform(crs = 4269)
industrial <- st_read("Data/Processed Data/Industrial DCAD Data.geojson")%>%
  st_transform(crs = 4269)
features_df <- read_csv("Data/features_df.csv") 
Dallas_Library <- read_csv("Data/Dallas Library.csv")
Dallas_Bus_Stops <- read_csv("Data/Dallas Bus Stops.csv")
Dallas_Rec <- read_csv("Data/Dallas Rec.csv")
dallas_county <- counties(state = "TX") %>%
  filter(GEOID == "48113") %>%
  st_transform(crs = 4269)
## add school data

# subset only school data

schools_df <- subset(features_df, Type %in% c("Primary Education", "Secondary Education",
                                              "Primary/Secondary Education", "Private Education"))

# Convert the school coordinates to an sf object

schools_sf <- st_as_sf(schools_df, coords = c("X", "Y"), crs = 4269) %>%
  .[dallas_county, ]
plot(schools_sf["Zip"])
# Buffer the school locations by 1/4 mile

schools_buffer <- st_buffer(schools_sf, dist = 402.336)
plot(schools_buffer["Zip"])
# Check if each parcel is within the 500-foot buffer zone of any school

school_final <- vacant_geo %>%
  mutate(School_Flag = st_intersects(geometry, schools_buffer, sparse = FALSE) %>% rowSums() > 0)

plot(school_final["School_Flag"])

## Add grocery store data

# subset only grocery data

groc_df <- subset(features_df, Type == "Grocery Store") 
# Convert the grocery coordinates to an sf object

groc_sf <- st_as_sf(groc_df, coords = c("X", "Y"), crs = 4269) %>%
  .[dallas_county, ]
plot(groc_sf["Zip"])
# Buffer the grocery locations by 1/4 mile

groc_buffer <- st_buffer(groc_sf, dist = 402.336)
plot(groc_buffer["Zip"])
# Check if each parcel is within the 500-foot buffer zone of any grocery store

groc_final <- school_final %>%
  mutate(Grocery_Flag = st_intersects(geometry, groc_buffer, sparse = FALSE) %>% rowSums() > 0)

## add rail data

# subset only rail features

rail_df <- subset(features_df, Type %in% c("Commuter Rail Station", "Light Rail Station",
                                           "Park and Ride"))


# Convert the rail coordinates to an sf object

rail_sf <- st_as_sf(rail_df, coords = c("X", "Y"), crs = 4269) %>%
  .[dallas_county, ]
plot(rail_sf["Zip"])
# Buffer the rail locations by 1/4 mile

rail_buffer <- st_buffer(rail_sf, dist = 402.336)
plot(rail_buffer["Zip"])
# Check if each parcel is within the 500-foot buffer zone of any rail

rail_final <- groc_final %>%
  mutate(Rail_Station_Flag = st_intersects(geometry, rail_buffer, sparse = FALSE) %>% rowSums() > 0)

## add park data

# Load park features

park_sf <- Dallas_Parks

# Ensure the geometries are valid
park_sf <- st_make_valid(park_sf)

# Buffer the park locations by 1/4 mile

park_buffer <- st_buffer(park_sf, dist = 402.336)

# Check if each parcel is within the 500-foot buffer zone of any park

park_final <- rail_final %>%
  mutate(Park_Flag = st_intersects(geometry, park_buffer, sparse = FALSE) %>% rowSums() > 0)

## add clinic data

# subset only clinic features

clinic_df <- subset(features_df, Type %in% c("Clinic", "Medical"))


# Convert the clinic coordinates to an sf object

clinic_sf <- st_as_sf(clinic_df, coords = c("X", "Y"), crs = 4269) %>%
  .[dallas_county, ]

# Buffer the clinic locations by 1/4 mile

clinic_buffer <- st_buffer(clinic_sf, dist = 402.336)

# Check if each parcel is within the 500-foot buffer zone of any clinic

clinic_final <- park_final %>%
  mutate(Clinic_Flag = st_intersects(geometry, clinic_buffer, sparse = FALSE) %>% rowSums() > 0)

## add rec center data

# Convert the city rec center coordinates to an sf object

rec_sf1 <- st_as_sf(Dallas_Rec, coords = c("X", "Y"), crs = 4269) %>%
  .[dallas_county, ]

# subset only county rec center features

rec_df <- subset(features_df, Type == "Community Center")

# Convert the county rec center coordinates to an sf object

rec_sf2 <- st_as_sf(rec_df, coords = c("X", "Y"), crs = 4269) %>%
  .[dallas_county, ]

# Create a buffer around the city combined features

rec_buffer1 <- st_buffer(rec_sf1, dist = 402.336)

# Create a buffer around the city combined features

rec_buffer2 <- st_buffer(rec_sf2, dist = 402.336)

# Check if each parcel is within the 500-foot buffer zone of any city rec center 

rec_final0 <- clinic_final %>%
  mutate(Rec_Center_Flag1 = st_intersects(geometry, rec_buffer1, sparse = FALSE) %>% rowSums() > 0)

# Check if each parcel is within the 500-foot buffer zone of any county rec center 

rec_final <- rec_final0 %>%
  mutate(Rec_Center_Flag2 = st_intersects(geometry, rec_buffer2, sparse = FALSE) %>% rowSums() > 0)

## add bus data

# Convert the bus coordinates to an sf object

bus_sf <- st_as_sf(Dallas_Bus_Stops, coords = c("X", "Y"), crs = 4269) %>%
  .[dallas_county, ]

# Buffer the bus locations by 1/4 mile

bus_buffer <- st_buffer(bus_sf, dist = 402.336)

# Check if each parcel is within the 500-foot buffer zone of any bus 

bus_final <- rec_final %>%
  mutate(Bus_Stop_Flag = st_intersects(geometry, bus_buffer, sparse = FALSE) %>% rowSums() > 0)

## add library data

# subset only library features

library_df <- subset(features_df, Type == "Library")

# Convert the library coordinates to an sf object

library_sf <- st_as_sf(library_df, coords = c("X", "Y"), crs = 4269) %>%
  .[dallas_county, ]

# Buffer the library locations by 1/4 mile

library_buffer <- st_buffer(library_sf, dist = 402.336)

# Check if each parcel is within the 500-foot buffer zone of any library

library_final <- bus_final %>%
  mutate(Library_Flag = st_intersects(geometry, library_buffer, sparse = FALSE) %>% rowSums() > 0)

## add environmental hazard data

# subset only environmental hazard data

pp_sf <- industrial

# Buffer the environmental hazard locations by 1/4 mile

pp_buffer <- st_buffer(pp_sf, dist = 402.336)

# Check if each parcel is within the 500-foot buffer zone of any environmental hazard

features_final <- library_final %>%
  mutate(ENV_HAZ_Flag = st_intersects(geometry, pp_buffer, sparse = FALSE) %>% rowSums() > 0)

# combine rec center columns 

features_final$Rec_Center_Flag <- features_final$Rec_Center_Flag1 | features_final$Rec_Center_Flag2

# combine transit columns 

features_final$Transit_Flag <- features_final$Bus_Stop_Flag | features_final$Rail_Station_Flag

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
features_final <- categorize_parcel(features_final)

# remove unnecessary columns
features_final <- features_final %>%
  select(-Rec_Center_Flag1, -Rec_Center_Flag2, -Bus_Stop_Flag, -Rail_Station_Flag, -features_count, -MULTIPLE_ACCOUNTS)


# load dataset into working directory
st_write(features_final, "Data/Processed Data/Processed Vacant Land Parcels with Additional Data Appended.geojson", delete_dsn = TRUE)
