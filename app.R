#
# This is the server logic of a Shiny web application. You can run the
# application by clicking "Run App" above.

library(shiny)
library(shinyWidgets)
library(htmltools)
library(sf)
library(stringr)
library(shinythemes)
library(shinymanager)

# Source page UIs
source("about_page.R")
source("forecasts_page.R")
source("prediction_page.R")
source("secured_nest_page.R")
source("functions.R")
source("load_data.R")

# Define thumbnail dir
# Source additional pages

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("readable"),
  # Navbar to each page
  navbarPage(
    "Everglades Wading Birds",
    tabPanel("Species Detection", uiOutput("predicted")),
    tabPanel("Species Forecasts", uiOutput("forecasts")),
    tabPanel("Current Nests",uiOutput('current_nests')),
    tabPanel("About", uiOutput("about"))
  )
)




# Define server logic required
server <- function(input, output, session) {
  output$zooniverse_anotation <- renderPlot(zooniverse_complete())

  # Setmapbox key
  readRenviron("source_token.txt")
  MAPBOX_ACCESS_TOKEN <- Sys.getenv("MAPBOX_ACCESS_TOKEN")

  ## Secured Nest Page ##
  # define some basic credentials (on data.frame)
  credentials <- data.frame(
    user = c("shiny", "shinymanager"), # mandatory
    password = c("azerty", "12345"), # mandatory
    start = c("2019-04-15"), # optional (all others)
    expire = c(NA, "2019-12-31"),
    admin = c(FALSE, TRUE),
    comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE
  )

  # Create pages
  output$about <- about_page()
  output$predicted <- predicted_page(df)
  output$forecasts <- forecasts_page()
  output$current_nests<-current_nest_page(df)

  # authentication module
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )

  output$res_auth <- renderPrint({
    reactiveValuesToList(auth)
  })

  #### Sidebar Map###
  output$map <- create_map(colonies)

  observe({
    new_map_data <- map_filter()
    leafletProxy("map", data = new_map_data) %>%
      clearMarkers() %>%
      addMarkers(
        popup =
          ~site
      )
  })

  map_filter <- reactive({
    # filter based on selection
    if (is.null(input$prediction_site)) {
      return(colonies)
    }

    map_data <- colonies %>% filter(site == input$prediction_site)
    return(map_data)
  })

  ## Prediction panel##
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

  site_name_filter <- reactive({
    return(input$prediction_site)
  })

  species_name_filter <- reactive({
    if ("All" %in% input$prediction_species) {
      return("All")
    } else {
      return(input$prediction_species)
    }
  })

  output$date_slider <- renderUI({
    print(paste("selected_size is:", site_name_filter()))
    selected_df <- df %>%
      filter(site == site_name_filter())
    available_dates <- sort(unique(selected_df$event))
    sliderTextInput(inputId = "mapbox_date", "Select Date", choices = available_dates)
  })

  output$predicted_time_plot <-
    renderPlot(
      time_predictions(
        df,
        site_name_filter(),
        species = species_name_filter(),
        selected_event = input$mapbox_date
      )
    )
  output$sample_prediction_map <-
    renderLeaflet(plot_predictions(df = prediction_filter(), MAPBOX_ACCESS_TOKEN))





  output$pred_obs_Image <- renderImage({
    filename <- normalizePath(file.path("./forecasts",
                              paste0("nb_origin_", input$forecast_origin, ".png")))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Observed as a function of predicted for ", input$origin))

  }, deleteFile = FALSE)



  output$greg_Image <- renderImage({
    filename <- normalizePath(file.path("./forecasts",
                              paste0("greg_nb_origin_", input$forecast_origin, ".png")))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Time series for GREG since ",  input$forecast_origin))

  }, deleteFile = FALSE)


  output$wost_Image <- renderImage({
    filename <- normalizePath(file.path("./forecasts",
                              paste0("wost_nb_origin_", input$forecast_origin, ".png")))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Time series for WOST since ",  input$forecast_origin))

  }, deleteFile = FALSE)


  output$whib_Image <- renderImage({
    filename <- normalizePath(file.path("./forecasts",
                              paste0("whib_nb_origin_", input$forecast_origin, ".png")))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Time series for WHIB since ",  input$forecast_origin))

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
