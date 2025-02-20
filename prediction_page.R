predicted_page <- function(df) {
  renderUI({
    # Get available species from the data
    available_species <- sort(unique(df$label))

    fluidRow(
      column(4,
        selectInput(
          "prediction_site",
          "Select Site",
          choices = c("All", unique(colonies$site)),
          selected = "Joule"
        ),
        selectInput(
          "prediction_species",
          "Select Species",
          choices = c("All", available_species), # Use dynamic species list
          multiple = TRUE,
          selected = "All"
        ),
        uiOutput("date_slider"),
        leafletOutput("map", height = "400px")
      ),
      column(8,
        plotOutput("predicted_time_plot", height = "400px"),
        leafletOutput("sample_prediction_map", height = "400px")
      )
    )
  })
}
