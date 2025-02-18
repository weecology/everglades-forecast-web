# functions
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(gridExtra)
library(stringr)
library(htmltools)
library(tidyr)
library(RColorBrewer)

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
  # Get unique species from the data
  available_species <- sort(unique(df$label))
  
  # Create dynamic color palette using our robust function
  species_colors <- species_colors(df)
  
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

time_predictions <- function(df, selected_site, selected_species = "All", selected_event = NULL) {
  df <- data.frame(df)

  # Check if the selected site is "All"
  if (selected_site == "All") {
    # Return a blank plot
    return(ggplot() +
             geom_blank() +
             theme_void())
  }

  # Get unique species from the data
  available_species <- sort(unique(df$label))

  # Grouping by site, event, and label (species)
  if ("All" %in% selected_species) {
    g <- df %>%
      filter(site == selected_site) %>%
      group_by(site, event, year, label) %>%
      summarize(n = n(), .groups = "drop")
  } else {
    g <- df %>%
      filter(site == selected_site, label %in% selected_species) %>%
      group_by(site, event, year, label) %>%
      summarize(n = n(), .groups = "drop")
  }

  # Create a dynamic color palette based on number of species
  n_species <- length(available_species)
  species_colors <- colorFactor(
    palette = colorRampPalette(brewer.pal(8, "Set2"))(n_species),
    domain = available_species,
    ordered = TRUE
  )

  # Plotting with different colors for each species
  if (nrow(g) > 0) {
    ggplot(g, aes(x = event, y = n, color = label, group = label)) +
      geom_line() +
      geom_point(size = 2) +
      # Add highlighted point if a date is selected
      {if (!is.null(selected_event)) 
        geom_point(data = subset(g, event == selected_event),
                  size = 8,
                  color = "#00CC00",
                  alpha = 0.7)} +
      labs(y = "Detected Birds", 
           x = "Date",
           color = "Species") +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 14)
      ) +
      facet_wrap(nrow = 1, ~ year, scales = "free_x") +
      scale_color_manual(values = species_colors(available_species))  # Use dynamic colors
  } else {
    ggplot() +
      geom_blank() +
      theme_void()
  }
}

# Update the species_colors function to be more robust
species_colors <- function(df) {
  available_species <- sort(unique(df$label))
  n_species <- length(available_species)
  
# Define a set of distinct colors (adjusted to remove tree, grass, and soil-like colors)
base_colors <- c("#E41A1C", "#377EB8", "#984EA3", "#FF7F00",  
                 "#FFFF33", "#F781BF", "#8B4513", "#00CED1")
  # If we need more colors, use colorRampPalette
  if (n_species > length(base_colors)) {
    colors <- colorRampPalette(base_colors)(n_species)
  } else {
    colors <- base_colors[1:n_species]
  }
  
  colorFactor(
    palette = colors,
    domain = available_species,
    ordered = TRUE
  )
}

# Construct mapbox url
construct_id <- function(site, event) {
  event_formatted <- format(event, "%m_%d_%Y")
  tileset_id <- paste(site, "_", event_formatted, sep = "")
  return(tileset_id)
}
