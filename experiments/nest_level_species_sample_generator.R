library(dplyr)
library(sf)

unzip("../data/nest_detections_processed.zip", exdir = "data")

nests <- st_read("data/nest_detections_processed.shp") %>%
  st_transform(4326) %>%
  rename(site = Site, year = Year) %>%
  filter(year != "2021.shp") # Old pre-species file with bad filename
nest_coords <- st_coordinates(nests) %>%
  as.data.frame() %>%
  select(lat = Y, long = X)
nests <- nests %>%
  as.data.frame() %>%
  bind_cols(nest_coords)

set.seed(26)

samples <- nests %>%
  as.data.frame() %>%
  group_by(site, year, species) %>%
  slice(sample(1:10)) %>%
  slice(sample(1:n())) %>% # Randomize order to not give clues to species ID
  ungroup() %>%
  mutate(sample_id = seq_len(n())) %>%
  select(site, year, sample_id, nest_id, species, lat, long)

write.csv(samples, "./experiments/nest_level_species_sample_locations.csv", row.names=FALSE)
