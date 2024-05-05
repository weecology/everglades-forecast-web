#Prepare data. This script is run once in the folder to reduce the load time.
library(sf)
library(dplyr)
source("functions.R")


# Set thresholds
min_confidence <- 0.4
### Preprocess .shp once to improve load time. This code needs to be integrated upstream.
unzip("data/PredictedBirds.zip", exdir = "data")
df <- st_read("data/PredictedBirds.shp")
df <- df %>% filter(
  Site %in% c(
    "6thBridge",
    "CypressCity",
    "Jerrod",
    "JetportSouth",
    "Joule",
    "StartMel",
    "Vacation"
  )
) %>%
  select(-xmin, -ymin, -xmax, -ymax)
df$event <- as.Date(df$Date, "%m_%d_%Y")
df$tileset_id <- construct_id(df$Site, df$event)
df <- df %>% filter(score > min_confidence)
df <- st_transform(df, 4326)

df <-
  mutate(df, bird_id = row_number()) %>% rename(year = Year, site = Site)

write_sf(df, "data/PredictedBirds.shp")
