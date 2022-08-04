library(sf)
library(dplyr)
source("functions.R")

# Set thresholds
min_confidence <- 0.4

#Predictions
df <- st_read("data/PredictedBirds.shp")
colonies <- df %>% group_by(site) %>% slice(1) %>% st_centroid() %>% st_transform(4326)

