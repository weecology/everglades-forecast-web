library(sf)
library(dplyr)
source('functions.R')

# Set thresholds
min_confidence <- 0.4

#Load data
raw_data <- load_classifications()
selected_boxes <- filter_annotations(raw_data)
colonies <- st_read(
  "data/colonies.csv",
  options = c("X_POSSIBLE_NAMES=longitude","Y_POSSIBLE_NAMES=latitude"))
samples <- st_read(
  "./experiments/no_experiment.csv",
  options = c("X_POSSIBLE_NAMES=long", "Y_POSSIBLE_NAMES=lat"),
  crs = 4326)

#Predictions
unzip("data/PredictedBirds.zip", exdir = "data")
df <- st_read("data/PredictedBirds.shp")
df$event <- as.Date(df$event,"%m_%d_%Y")
df$tileset_id <- construct_id(df$site,df$event)
df <- df %>% filter(score > min_confidence)
df <- st_transform(df, 4326)
df <- st_centroid(df)
year <- sapply(df$event, function(event) str_split(event, "-")[[1]][[1]])
df <- mutate(df, bird_id = row_number(), year = year)

#Nest predictions
unzip("data/nest_detections_processed.zip", exdir = "data")
nestdf <- st_read("data/nest_detections_processed.shp")
nestdf$first_obs <- as.Date(nestdf$first_obs,"%m_%d_%Y")
nestdf$last_obs <- as.Date(nestdf$last_obs,"%m_%d_%Y")
nestdf <- st_centroid(nestdf)
nestdf <- st_transform(nestdf,4326)
