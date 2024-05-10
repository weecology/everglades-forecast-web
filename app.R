#
# This is the server logic of a Shiny web application. You can run the
# application by clicking "Run App" above.

library(shiny)
library(shinyWidgets)
library(htmltools)
library(sf)
library(stringr)
library(shinythemes)

# Source page UIs
source("about_page.R")
source("forecasts_page.R")
source("prediction_page.R")
source("functions.R")
source("load_data.R")

ui <- fluidPage(
  theme = shinytheme("readable"),
  # Navbar to each page
  navbarPage(
    "Everglades Wading Birds",
    tabPanel("Species Detection", uiOutput("predicted")),
    tabPanel("Species Forecasts", uiOutput("forecasts")),
    tabPanel("About", uiOutput("about"))
  )
)

server <- function(input, output, session) {
  output$zooniverse_anotation <- renderPlot(zooniverse_complete())

  # Set mapbox key
  if (file.exists("source_token.txt"))
    readRenviron("source_token.txt")
  MAPBOX_ACCESS_TOKEN = Sys.getenv("MAPBOX_ACCESS_TOKEN")
  if (is.na(MAPBOX_ACCESS_TOKEN) || MAPBOX_ACCESS_TOKEN == "")
    paste("Set MAPBOX ACCESS TOKEN,", "Refer to the README.")

  # Create pages
  output$about <- about_page()
  output$predicted <- predicted_page(df)
  output$forecasts <- forecasts_page()

  #### Sidebar Map###
  output$map <- create_map(colonies)

  observe({
    new_map_data <- map_filter()
    leafletProxy("map", data = new_map_data) %>%
      clearMarkers() %>%
      addMarkers(popup = ~ site)
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
    # filter based on selection
    if (is.null(input$prediction_site)) {
      return(colonies)
    }
    map_data <- colonies %>% filter(site == input$prediction_site)
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

    # Check if the selected site is "All", don't render the slider
    if (selected_site == "All") {
      return(NULL)
    } else {
      # Check if the selected date is in the available dates
      selected_date <- input$mapbox_date
      if (is.null(selected_date) || !(selected_date %in% available_dates)) {
        # If the selected date is null or not in the available dates,
        # set it to the first available date
        selected_date <- available_dates[1]
      }

      # Render the slider
      sliderTextInput(
        inputId = "mapbox_date",
        label = "Select Date",
        choices = available_dates,
        selected = selected_date
      )
    }
  })


  output$predicted_time_plot <-
    renderPlot(
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
      alt = paste("Time series for GREG since ",  input$forecast_origin)
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
      alt = paste("Time series for WOST since ",  input$forecast_origin)
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
      alt = paste("Time series for WHIB since ",  input$forecast_origin)
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
}

# Run the application
shinyApp(ui = ui, server = server)
