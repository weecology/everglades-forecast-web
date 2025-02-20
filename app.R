# Install required packages if not already installed
if (!require("leaflet.extras"))
  install.packages("leaflet.extras")
if (!require("leaflet.extras2"))
  install.packages("leaflet.extras2")
if (!require("leaflet.mapboxgl")) {
  remotes::install_github("rstudio/leaflet.mapboxgl")
}

# Then load the libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(htmltools)
  library(sf)
  library(stringr)
  library(shinythemes)
  library(shinyjs) # For JavaScript operations
  library(lubridate) # Add this for year() function
  library(memoise) # Added for caching
  library(leaflet.extras)
  library(leaflet.extras2)
  library(leaflet.mapboxgl)
})

# Source page UIs
source("about_page.R")
source("forecasts_page.R")
source("prediction_page.R")
source("functions.R")
source("load_data.R")
source("dev_season_page.R")

# Add near the top of the file
options(shiny.maxRequestSize = 30 * 1024 ^ 2) # Increase max request size to 30MB
options(shiny.reactlog = FALSE)  # Disable reactive log in production
options(shiny.autoreload = FALSE)  # Disable auto-reload in production

ui <- fluidPage(
  useShinyjs(),
  # Initialize shinyjs
  theme = shinytheme("readable"),
  tags$head(tags$style(
    HTML(
      "
      #loading-content {
        position: absolute;
        background: #000000;
        opacity: 0.9;
        z-index: 100;
        left: 0;
        right: 0;
        height: 100%;
        text-align: center;
        color: #FFFFFF;
      }
    "
    )
  ), # Add the JavaScript function for clearTimeout
  tags$script(
    HTML(
      "
      var timers = {};
      Shiny.addCustomMessageHandler('clearTimer', function(id) {
        if (timers[id]) {
          clearTimeout(timers[id]);
          delete timers[id];
        }
      });
      Shiny.addCustomMessageHandler('setTimer', function(data) {
        timers[data.id] = setTimeout(function() {
          Shiny.setInputValue('timer_triggered_' + data.id, true, {priority: 'event'});
        }, data.delay);
      });
    "
    )
  )),
  div(id = "loading-content", "Loading..."),
  # Navbar to each page
  navbarPage(
    "Everglades Wading Birds",
    tabPanel("Species Detection", uiOutput("predicted")),
    tabPanel("Species Forecasts", uiOutput("forecasts")),
    tabPanel("About", uiOutput("about")),
    tabPanel("Current Season", uiOutput("dev_season"))  # Changed from "Dev Season"
  )
)

server <- function(input, output, session) {
  # Add this near the top of the server function
  # Set mapbox key
  if (file.exists("source_token.txt")) {
    readRenviron("source_token.txt")
  }
  MAPBOX_ACCESS_TOKEN = Sys.getenv("MAPBOX_ACCESS_TOKEN")
  if (is.na(MAPBOX_ACCESS_TOKEN) || MAPBOX_ACCESS_TOKEN == "") {
    stop("Set MAPBOX ACCESS TOKEN, Refer to the README.")
  }

  # Cache for precomputed data
  cached_data <- reactiveVal(NULL)

  # Initialize cache on startup
  observe({
    # This will run once when the app starts
    year_to_filter <- as.numeric(current_year())

    # Precompute yearly data with better NA handling and optimization
    yearly_data <- df %>%
      filter(lubridate::year(as.Date(event)) == year_to_filter) %>%
      # Handle NAs and add computed columns
      mutate(
        month = lubridate::month(as.Date(event)),
        week = lubridate::week(as.Date(event)),
        date = as.Date(event),
        # Ensure site and label are factors for better performance
        site = factor(site),
        label = factor(label),
        # Add required columns for plot_predictions
        score = 1.0,
        tileset_id = paste(site, event, sep = "_")
      ) %>%
      # Remove rows with NA in critical columns
      filter(!is.na(site), !is.na(label), !is.na(event))

    # Get all unique combinations
    all_sites <- c("All", unique(yearly_data$site))
    all_species <- c("All", unique(yearly_data$label))
    all_dates <- unique(yearly_data$date)

    # Pre-calculate site-species combinations
    site_species_combinations <- expand.grid(site = all_sites,
                                             species = all_species,
                                             stringsAsFactors = FALSE)

    # Pre-calculate aggregated data for all combinations
    aggregated_data <- yearly_data %>%
      # First, calculate counts for each site-species-date combination
      group_by(site, label, date) %>%
      summarise(count = n(), .groups = "drop") %>%
      # Complete the dataset with all combinations
      complete(
        site = unique(yearly_data$site),
        label = unique(yearly_data$label),
        date = all_dates,
        fill = list(count = 0)
      )

    # Pre-calculate "All" site summaries
    all_sites_data <- aggregated_data %>%
      group_by(label, date) %>%
      summarise(count = sum(count), .groups = "drop")

    # Pre-calculate "All" species summaries
    all_species_data <- aggregated_data %>%
      group_by(site, date) %>%
      summarise(count = sum(count), .groups = "drop")

    # Pre-calculate map data with optimization
    map_data <- yearly_data %>%
      select(site, label, event, geometry, score, tileset_id) %>%
      st_transform(4326) %>%
      st_simplify(dTolerance = 0.0001) %>%
      filter(st_is_valid(.))

    # Store everything in cache
    cache_list <- list(
      yearly_data = yearly_data,
      aggregated_data = aggregated_data,
      all_sites_data = all_sites_data,
      all_species_data = all_species_data,
      map_data = map_data,
      site_species_combinations = site_species_combinations,
      available_dates = sort(all_dates),
      available_sites = all_sites,
      available_species = all_species
    )

    cached_data(cache_list)
  })

  # Get current year with development fallback
  current_year <- reactive({
    # Check if we're in production environment
    in_production <- Sys.getenv("INPRODUCTION", "FALSE")

    if (tolower(in_production) == "true") {
      # In production: use actual current year
      format(Sys.Date(), "%Y")
    } else {
      # In development: return 2023
      "2022"
    }
  })

  # Load credentials from YAML file
  credentials <- yaml::read_yaml("~/.credentials.yml")$credentials

  # Authentication state
  is_authenticated <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)

  # Initialize colonies data
  colonies_data <- reactive({
    # Get the raw colonies data
    data <- colonies  # directly use the colonies data frame

    # Ensure coordinates are numeric and in the correct format
    if (inherits(data, "sf")) {
      coords <- st_coordinates(data)
      data$longitude <- coords[, 1]
      data$latitude <- coords[, 2]
    }
    data
  })

  # Add a reactive for yearly data to avoid recomputing
  current_year_data <- reactive({
    year_to_filter <- as.numeric(current_year())
    df %>%
      filter(lubridate::year(as.Date(event)) == year_to_filter)
  })

  # Update filtered_site_data to use pre-calculated data
  filtered_site_data <- reactive({
    req(is_authenticated())
    req(cached_data())

    selected_site <- current_season_site_filter()
    selected_species <- current_season_species_filter()

    # Get the appropriate pre-calculated data
    if (selected_site == "All" && "All" %in% selected_species) {
      # Both "All" selected - use yearly totals by species
      data <- cached_data()$all_sites_data
    } else if (selected_site == "All") {
      # All sites, specific species
      data <- cached_data()$aggregated_data %>%
        filter(label %in% selected_species) %>%
        group_by(label, date) %>%
        summarise(count = sum(count), .groups = "drop")
    } else if ("All" %in% selected_species) {
      # Specific site, all species
      data <- cached_data()$all_species_data %>%
        filter(site == selected_site)
    } else {
      # Specific site and species
      data <- cached_data()$aggregated_data %>%
        filter(site == selected_site, label %in% selected_species)
    }

    # Ensure we have data
    if (nrow(data) == 0) {
      return(
        data.frame(
          date = as.Date(character()),
          count = numeric(),
          label = character(),
          stringsAsFactors = FALSE
        )
      )
    }

    data
  })

  # Add a reactive value to track current view (plot or map)
  current_view <- reactiveVal("plot")

  # Update current_view when switching between plot and map
  observe({
    # Set to "plot" when plot is visible
    if (!is.null(input$current_season_plot_visible) &&
          input$current_season_plot_visible) {
      current_view("plot")
    }
    # Set to "map" when map is visible
    if (!is.null(input$current_season_prediction_map_visible) &&
          input$current_season_prediction_map_visible) {
      current_view("map")
    }
  })

  # Update the UI layout to match Species Detection tab
  output$login_or_data <- renderUI({
    req(colonies_data())
    if (!is_authenticated()) {
      uiOutput("login_ui")
    } else {
      fluidRow(column(
        12,
        div(
          style = "float: right;",
          actionButton("logout", "Logout", class = "btn-danger")
        ),
        h3(paste(
          "Current Season Analysis (", current_year(), ")"
        )),
        fluidRow(
          column(
            4,
            selectInput(
              "current_season_site",
              "Select Site",
              choices = c("All", unique(colonies_data()$site)),
              selected = "Joule"
            ),
            selectInput(
              "current_season_species",
              "Select Species",
              choices = c("All", "Great Egret", "Wood Stork", "White Ibis"),
              multiple = TRUE,
              selected = "All"
            ),
            uiOutput("current_season_date_slider"),
            leafletOutput("current_season_map", height = "400px")
          ),
          column(8, tabsetPanel(
            tabPanel(
              "Time Series",
              plotOutput("current_season_plot", height = "400px")
            ),
            tabPanel(
              "Bird Detections",
              leafletOutput("current_season_prediction_map", height = "800px")
            )
          ))
        )
      ))
    }
  })

  # Handle logout
  observeEvent(input$logout, {
    is_authenticated(FALSE)
    current_user(NULL)
  })

  # Update the reactive filters to match Species Detection tab
  current_season_site_filter <- reactive({
    return(as.character(input$current_season_site))
  })

  current_season_species_filter <- reactive({
    if ("All" %in% input$current_season_species) {
      return("All")
    } else {
      return(input$current_season_species)
    }
  })

  # Update date slider to use cache
  output$current_season_date_slider <- renderUI({
    req(is_authenticated())
    req(cached_data())

    selected_site <- current_season_site_filter()

    if (selected_site == "All") {
      return(NULL)
    }

    # Use cached dates for the selected site
    available_dates <- cached_data()$yearly_data %>%
      filter(site == selected_site) %>%
      pull(date) %>%
      unique() %>%
      sort()

    if (length(available_dates) > 0) {
      selected_date <- input$current_season_date
      if (!is.null(selected_date) &&
            !(selected_date %in% available_dates)) {
        selected_date <- available_dates[1]
      }

      sliderTextInput(
        inputId = "current_season_date",
        label = "Select Date",
        choices = available_dates,
        selected = selected_date
      )
    }
  })

  # Current Season Map
  output$current_season_map <- renderLeaflet({
    req(is_authenticated())
    data <- isolate(colonies_data())

    leaflet(data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~site,
        label = ~site,
        layerId = ~site
      ) %>%
      setView(
        lng = mean(data$longitude, na.rm = TRUE),
        lat = mean(data$latitude, na.rm = TRUE),
        zoom = 8
      )
  })

  # Update plot rendering to use pre-aggregated data
  output$current_season_plot <- renderPlot({
    req(is_authenticated())
    req(cached_data())

    # Get current selections
    selected_site <- current_season_site_filter()
    selected_species <- current_season_species_filter()

    # Use pre-aggregated data
    plot_data <- cached_data()$aggregated_data

    # Apply filters
    if (selected_site != "All") {
      plot_data <- plot_data %>% filter(site == selected_site)
    }
    if (!"All" %in% selected_species) {
      plot_data <- plot_data %>% filter(label %in% selected_species)
    }

    if (nrow(plot_data) == 0) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No data available for current selection",
            size = 6
          ) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      )
    }

    # Create the plot using pre-aggregated data
    tryCatch({
      if (selected_site == "All") {
        # For all sites, sum across sites
        plot_data <- plot_data %>%
          group_by(date, label) %>%
          summarise(count = sum(count), .groups = "drop")
      }

      ggplot(plot_data,
             aes(
               x = date,
               y = count,
               color = label,
               group = label
             )) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
        labs(
          x = "Date",
          y = "Number of Birds",
          color = "Species",
          title = if (selected_site == "All")
            "Bird Counts Across All Sites"
          else
            paste("Bird Counts at", selected_site)
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        )
    }, error = function(e) {
      print(paste("Error in plot:", e$message))
      ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Error creating plot",
          size = 6
        ) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    })
  })

  # Hide loading message on app start
  observe({
    shinyjs::hide("loading-content")
  })

  # Show/hide loading message during map updates
  observeEvent(input$current_season_site, {
    shinyjs::show("loading-content")
    shinyjs::delay(300, shinyjs::hide("loading-content"))
  })

  # Create pages
  output$about <- about_page()
  output$predicted <- predicted_page(df)
  output$forecasts <- forecasts_page()
  output$dev_season <- dev_season_page(
    df = df,
    is_authenticated = is_authenticated,
    current_user = current_user,
    current_year = current_year
  )

  #### Sidebar Map###
  output$map <- renderLeaflet({
    data <- colonies_data()

    leaflet(data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~site,
        label = ~site,
        layerId = ~site
      ) %>%
      setView(
        lng = mean(data$longitude, na.rm = TRUE),
        lat = mean(data$latitude, na.rm = TRUE),
        zoom = 8
      )
  })

  site_name_filter <- reactive({
    return(as.character(input$prediction_site))
  })

  species_name_filter <- reactive({
    if ("All" %in% input$prediction_species) {
      return("All")
    } else {
      return(input$prediction_species)
    }
  })

  map_filter <- reactive({
    if (is.null(input$prediction_site)) {
      return(colonies_data())
    }
    map_data <- colonies_data() %>% filter(site == input$prediction_site)
    return(map_data)
  })

  ## Prediction panel ##
  prediction_filter <- reactive({
    if (is.null(input$mapbox_date)) {
      mapbox_date <- "2020-02-24"
    } else {
      mapbox_date <- input$mapbox_date
    }

    # filter based on selection
    print(paste("mapbox date is:", mapbox_date))
    print(paste("selected site is:", site_name_filter()))

    selected_species <- species_name_filter()
    if ("All" %in% selected_species) {
      to_plot <-
        df %>% filter(site == site_name_filter(), event == mapbox_date)
    } else {
      to_plot <-
        df %>% filter(
          site == site_name_filter(),
          event == mapbox_date,
          label %in% species_name_filter()
        )
    }
    return(to_plot)
  })

  output$date_slider <- renderUI({
    selected_site <- site_name_filter()
    selected_df <- df %>% filter(site == selected_site)
    available_dates <- sort(unique(selected_df$event))

    # Check if the selected date is in the available dates
    selected_date <- input$mapbox_date
    if (!is.null(selected_date) &&
          !(selected_date %in% available_dates)) {
      # If the selected date is not in the available dates,
      # set it to the first available date
      selected_date <- available_dates[1]
    }

    # Check if the selected site is "All", don't render the slider
    if (selected_site == "All") {
      return(NULL)
    } else {
      # Otherwise, render the slider
      sliderTextInput(
        inputId = "mapbox_date",
        label = "Select Date",
        choices = available_dates,
        selected = selected_date
      )
    }
  })

  output$predicted_time_plot <- renderPlot(
    time_predictions(
      df,
      site_name_filter(),
      selected_species = species_name_filter(),
      selected_event = input$mapbox_date
    )
  )

  output$sample_prediction_map <-
    renderLeaflet(plot_predictions(df = prediction_filter(), MAPBOX_ACCESS_TOKEN))

  output$pred_obs_Image <- renderImage({
    filename <- normalizePath(file.path(
      "./forecasts",
      paste0("nb_origin_", input$forecast_origin, ".png")
    ))
    # Return a list containing the filename and alt text
    list(
      src = filename,
      alt = paste("Observed as a function of predicted for ", input$origin)
    )
  }, deleteFile = FALSE)

  output$greg_Image <- renderImage({
    filename <- normalizePath(file.path(
      "./forecasts",
      paste0("greg_nb_origin_", input$forecast_origin, ".png")
    ))
    # Return a list containing the filename and alt text
    list(
      src = filename,
      alt = paste("Time series for GREG since ", input$forecast_origin)
    )
  }, deleteFile = FALSE)

  output$wost_Image <- renderImage({
    filename <- normalizePath(file.path(
      "./forecasts",
      paste0("wost_nb_origin_", input$forecast_origin, ".png")
    ))
    # Return a list containing the filename and alt text
    list(
      src = filename,
      alt = paste("Time series for WOST since ", input$forecast_origin)
    )
  }, deleteFile = FALSE)

  output$whib_Image <- renderImage({
    filename <- normalizePath(file.path(
      "./forecasts",
      paste0("whib_nb_origin_", input$forecast_origin, ".png")
    ))
    # Return a list containing the filename and alt text
    list(
      src = filename,
      alt = paste("Time series for WHIB since ", input$forecast_origin)
    )
  }, deleteFile = FALSE)

  output$greg_title <- renderText({
    "GREG Counts"
  })

  output$wost_title <- renderText({
    "WOST Counts"
  })

  output$whib_title <- renderText({
    "WHIB Counts"
  })

  output$pred_obs_title <- renderText({
    "Observed vs. Predicted Counts"
  })

  # Dev Season handlers - match Species Detection exactly but only 2023 data
  dev_site_name_filter <- reactive({
    req(is_authenticated())
    return(as.character(input$dev_prediction_site))
  })

  dev_species_name_filter <- reactive({
    req(is_authenticated())
    if ("All" %in% input$dev_prediction_species) {
      return("All")
    } else {
      return(input$dev_prediction_species)
    }
  })

  # Filter for current year data only
  dev_data <- reactive({
    req(is_authenticated())
    year_to_filter <- as.numeric(current_year())

    df %>%
      filter(year == year_to_filter)  # Use the current_year value
  })

  dev_prediction_filter <- reactive({
    req(is_authenticated())

    if (is.null(input$dev_mapbox_date)) {
      # Get the most recent date for the selected site
      filtered_data <- dev_data() %>%
        filter(site == dev_site_name_filter())

      if (nrow(filtered_data) > 0) {
        mapbox_date <- max(filtered_data$event)
      } else {
        mapbox_date <- max(dev_data()$event)  # Fallback to overall max date
      }
    } else {
      mapbox_date <- input$dev_mapbox_date
    }

    print(paste("dev mapbox date is:", mapbox_date))
    print(paste("dev selected site is:", dev_site_name_filter()))

    selected_species <- dev_species_name_filter()
    if ("All" %in% selected_species) {
      to_plot <- dev_data() %>%
        filter(site == dev_site_name_filter(), event == mapbox_date)
    } else {
      to_plot <- dev_data() %>%
        filter(
          site == dev_site_name_filter(),
          event == mapbox_date,
          label %in% dev_species_name_filter()
        )
    }
    return(to_plot)
  })

  # Date slider for Dev Season - only current dates
  output$dev_date_slider <- renderUI({
    selected_site <- dev_site_name_filter()
    selected_df <- dev_data() %>% filter(site == selected_site)
    available_dates <- sort(unique(selected_df$event))

    selected_date <- input$dev_mapbox_date
    if (!is.null(selected_date) &&
          !(selected_date %in% available_dates)) {
      selected_date <- available_dates[1]
    }

    if (selected_site == "All") {
      return(NULL)
    } else {
      sliderTextInput(
        inputId = "dev_mapbox_date",
        label = "Select Date",
        choices = available_dates,
        selected = selected_date
      )
    }
  })

  # Time series plot - only 2023 data
  output$dev_predicted_time_plot <- renderPlot({
    req(is_authenticated())
    time_predictions(
      dev_data(),
      dev_site_name_filter(),
      selected_species = dev_species_name_filter(),
      selected_event = input$dev_mapbox_date
    )
  })

  # Map outputs stay the same
  output$dev_map <- renderLeaflet({
    req(is_authenticated())
    data <- colonies_data()

    leaflet(data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~site,
        label = ~site,
        layerId = ~site
      ) %>%
      setView(
        lng = mean(data$longitude, na.rm = TRUE),
        lat = mean(data$latitude, na.rm = TRUE),
        zoom = 8
      )
  })

  # Prediction map - only 2023 data
  output$dev_sample_prediction_map <- renderLeaflet({
    req(is_authenticated())
    plot_predictions(df = dev_prediction_filter(), MAPBOX_ACCESS_TOKEN)
  })

  # Map click handler for Dev Season
  observeEvent(input$dev_map_marker_click, {
    click <- input$dev_map_marker_click
    if (!is.null(click)) {
      updateSelectInput(session, "dev_prediction_site", selected = click$id)
    }
  })

  # Handle dev season login
  observeEvent(input$dev_login, {
    if (!is.null(input$dev_username) && !is.null(input$dev_password)) {
      user_creds <- credentials[[input$dev_username]]
      if (!is.null(user_creds) &&
            user_creds$password == input$dev_password) {
        is_authenticated(TRUE)
        current_user(input$dev_username)
      }
    }
  })

  # Handle dev season logout
  observeEvent(input$dev_logout, {
    is_authenticated(FALSE)
    current_user(NULL)
  })

  # Dev login message
  output$dev_login_message <- renderText({
    if (!is.null(input$dev_login) && input$dev_login > 0) {
      if (!is_authenticated()) {
        "Invalid username or password"
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
