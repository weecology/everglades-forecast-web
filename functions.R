# functions
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(gridExtra)
library(stringr)
library(htmltools)
library(tidyr)

# Site map
create_map <- function(colonies) {
  m <-
    leaflet(data = colonies) %>%
    addTiles() %>%
    addMarkers(popup = ~ site)
  return(renderLeaflet(m))
}

# Load data
load_classifications <- function() {
  raw_data <- read_sf("data/everglades-watch-classifications.shp")
  st_crs(raw_data) <- 32617
  return(raw_data)
}

# Filter classification by spatial overlap

check_events <- function(x) {
  if (str_detect(x, "_")) {
    return(str_match(x, "(\\w+)_")[, 2])
  } else {
    return(x)
  }
}

filter_annotations <- function(raw_data) {
  selected_ids <- unique(raw_data$selected_i)

  # Majority rule for labels
  majority_rule <- raw_data %>%
    data.frame() %>% # Converting to a non-spatial data frame improves speed 100-200x
    group_by(selected_i, label) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    slice(1) %>%
    as.data.frame() %>%
    mutate(majority_class = label) %>%
    dplyr::select(selected_i, majority_class)

  selected_boxes <-
    raw_data %>%
    filter(selected_i %in% selected_ids) %>%
    inner_join(majority_rule) %>%
    filter(!is.na(event))

  # !!Temp hotfix!!! until events are seperated from dates
  selected_boxes$event[selected_boxes$event %in% "03112020"] <-
    gsub(x = selected_boxes$event[selected_boxes$event %in% "03112020"],
         pattern = "03112020",
         replacement = "03_11_2020")
  selected_boxes$event <- as.Date(selected_boxes$event, "%m_%d_%Y")
  selected_boxes$tileset_id <- construct_id(selected_boxes$site, selected_boxes$event)

  # get unique boxes among observers
  return(selected_boxes)
}

plot_annotations <- function(selected_boxes, MAPBOX_ACCESS_TOKEN) {
  pal <- colorFactor(palette = "Dark2",
                     domain = selected_boxes$species)

  selected_centroids <- st_centroid(selected_boxes)
  selected_centroids <- st_transform(selected_centroids, 4326)

  # Create mapbox tileset
  mapbox_tileset <- unique(selected_centroids$tileset_id)
  mapbox_tileset <- paste("bweinstein.", mapbox_tileset, sep = "")

  # Use addTiles and URL template for Mapbox Classic tiles
  mapbox_url_template <-
    paste0(
      "https://api.tiles.mapbox.com/v4/",
      mapbox_tileset,
      "/{z}/{x}/{y}.png?access_token=",
      MAPBOX_ACCESS_TOKEN
    )

  m <- leaflet(data = selected_centroids) %>%
    addTiles(
      urlTemplate = mapbox_url_template,
      options = tileOptions(
        minZoom = 18,
        maxNativeZoom = 24,
        maxZoom = 24
      )
    ) %>%
    addCircles(
      stroke = TRUE,
      color = ~ pal(species),
      fillOpacity = 0.1,
      popup = ~ htmlEscape(label)
    )
  return(m)
}

plot_predictions <- function(df, MAPBOX_ACCESS_TOKEN) {
  mapbox_tileset <- unique(df$tileset_id)
  mapbox_tileset <- paste("bweinstein.", mapbox_tileset, sep = "")

  # Use addTiles and URL template for Mapbox Classic tiles
  mapbox_url_template <-
    paste0(
      "https://api.tiles.mapbox.com/v4/",
      mapbox_tileset,
      "/{z}/{x}/{y}.png?access_token=",
      MAPBOX_ACCESS_TOKEN
    )

  m <- leaflet(data = df) %>%
    addTiles(
      urlTemplate = mapbox_url_template,
      options = tileOptions(
        minZoom = 18,
        maxNativeZoom = 24,
        maxZoom = 24
      )
    ) %>%
    addPolygons(
      stroke = TRUE,
      fillOpacity = 0.1,
      popup = ~ htmlEscape(paste(label, round(score, 2), sep = ":")),
      color = ~ species_colors(label)
    )
  return(m)
}

time_predictions <- function(df, select_site, selected_species = "All", selected_event = NA) {
  df <- data.frame(df)

  # Check if the selected site is "All"
  if (select_site == "All") {
    # Return a blank plot
    return(ggplot() +
             geom_blank() +
             theme_void())
  }

  # Grouping by site and event
  if ("All" %in% selected_species) {
    g <- df %>%
      filter(site == select_site) %>%
      group_by(site, event, year) %>%
      summarize(n = n(), .groups = "drop")
  } else {
    g <- df %>%
      filter(site == select_site, label %in% selected_species) %>%
      group_by(site, event, year, label) %>%
      summarize(n = n(), .groups = "drop")
  }

  # Plotting
  if (nrow(g) > 0) {
    ggplot(g, aes(x = event, y = n)) +
      geom_point(aes(color = factor(event == selected_event)), size = 1.5) +
      geom_line() +
      labs(y = "Detected Birds", x = "Date") +
      theme(text = element_text(size = 20)) +
      facet_wrap(nrow = 1, ~ year, scales = "free_x") +
      scale_color_manual(guide = "none", values = c("black", "red"))
  } else {
    ggplot() +
      geom_blank() +
      theme_void()
  }
}

species_colors <- colorFactor(
  palette = c("yellow", "blue",
              "#ff007f", "brown",
              "purple", "white"),
  domain = c(
    "Great Egret",
    "Great Blue Heron",
    "Roseate Spoonbill",
    "Wood Stork",
    "Snowy Egret",
    "White Ibis"
  ),
  ordered = TRUE
)

# Construct mapbox url
construct_id <- function(site, event) {
  event_formatted <- format(event, "%m_%d_%Y")
  tileset_id <- paste(site, "_", event_formatted, sep = "")
  return(tileset_id)
}
