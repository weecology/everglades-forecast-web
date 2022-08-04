#Prepare data. This script is run once in the folder to reduce the load time.
library(sf)
library(dplyr)
source('functions.R')

### Preprocess .shp once to improve load time. This code needs to be integrated upstream.
unzip("data/PredictedBirds.zip", exdir = "data")
df <- st_read("data/PredictedBirds.shp")
df <- df %>% filter(site %in% c("6thBridge","CypressCity","Jerrod","JetportSouth","Joule",
                                "StartMel","Vacation")) %>% select(-xmin, -ymin, -xmax, -ymax)
df$event <- as.Date(df$event,"%m_%d_%Y")
df$tileset_id <- construct_id(df$site,df$event)
df <- df %>% filter(score > min_confidence)
df <- st_transform(df, 4326)
year <- sapply(df$event, function(event) str_split(event, "-")[[1]][[1]])
df <- mutate(df, bird_id = row_number(), year = year)

write_sf(df, "data/PredictedBirds.shp")
