library(sf)
library(dplyr)

# Set thresholds
min_confidence <- 0.4
min_detections <- 3

#Load data
raw_data <- load_classifications()
selected_boxes <- filter_annotations(raw_data)
colonies <- st_read(
  "data/colonies.csv",
  options = c("X_POSSIBLE_NAMES=longitude","Y_POSSIBLE_NAMES=latitude"))
field_nests <- st_read(
  "https://raw.githubusercontent.com/weecology/EvergladesWadingBird/main/Nesting/UAV_Flagged_Nest_Coordinates_2021.csv",
  options = c("X_POSSIBLE_NAMES=long", "Y_POSSIBLE_NAMES=lat"),
  crs = 4326)
field_nests <- field_nests %>%
  rename(site = colony) %>%
  mutate(year = 2021)
field_nests$site <- trimws(field_nests$site) # There is currently an extra space in at least one value of site

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
unzip("data/nest_detections.zip", exdir = "data")
nestdf <- st_read("data/nest_detections.shp")
nestdf$Date <- as.Date(nestdf$Date,"%m_%d_%Y")
nestdf$tileset_id <- construct_id(nestdf$Site,nestdf$Date)
nestdf <- st_centroid(nestdf)
nestdf <- st_transform(nestdf,4326)
selected_indices <- nestdf %>%
                as.data.frame() %>%
                filter(score > min_confidence) %>%
                group_by(Site, target_ind) %>%
                summarize(n = n()) %>%
                filter(n >= min_detections) %>%
                mutate(site_index = paste(Site,target_ind)) 
nestdf <- nestdf %>%
  mutate(site_index = paste(Site,target_ind)) %>%
  inner_join(selected_indices)

# Add random bird locations to field nests

num_field_nests <- nrow(field_nests)
field_nest_sites <- unique(field_nests$site)
field_nest_years <- unique(field_nests$year)

set.seed(26) # Keep same nest number and fake nest locations across runs

random_birds <- df %>%
  filter(site %in% field_nest_sites, year %in% field_nest_years) %>%
  mutate(real_nest = "No") %>%
  select(site, year, species = label, real_nest) %>%
  slice(sample(1:num_field_nests))

field_nests <- field_nests %>%
  mutate(real_nest = "Yes") %>%
  select(site, year, species, real_nest) %>%
  rbind(random_birds) %>%
  slice(sample(1:n())) %>% # Randomize order to not give clues to real nests
  mutate(field_nest_id = seq_len(n()))

field_nests