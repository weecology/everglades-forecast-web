# Load necessary libraries
library(sf)
library(dplyr)

# Source custom functions
source("functions.R")
source("preprocess.R")

# Set thresholds
min_confidence <- 0.4


# Read prediction data
df <- st_read("data/PredictedBirds.shp")

# Extract centroids of colonies
colonies <- df %>%
  group_by(site) %>%
  slice(1) %>%
  st_centroid() %>%
  st_transform(4326)
