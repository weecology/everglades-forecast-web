library(sf)
library(dplyr)
source('functions.R')

# Set thresholds
min_confidence <- 0.4

#Load data
raw_data <- load_classifications()
selected_boxes <- filter_annotations(raw_data)
colonies <- selected_boxes %>% group_by(site) %>% slice(1) %>% st_centroid() %>% st_transform(4326)

#Predictions
unzip("data/PredictedBirds.zip", exdir = "data")
df <- st_read("data/PredictedBirds.shp")
df$event <- as.Date(df$event,"%m_%d_%Y")
df$tileset_id <- construct_id(df$site,df$event)
df <- df %>% filter(score > min_confidence)
df <- st_transform(df, 4326)
year <- sapply(df$event, function(event) str_split(event, "-")[[1]][[1]])
df <- mutate(df, bird_id = row_number(), year = year)