predicted_page<-function(df){
  unique(df$tiles)
  renderUI({
      sidebarLayout(sidebarPanel(leafletOutput("map")),
      mainPanel(
        selectInput("prediction_tileset","Event",choices = sort(unique(df$tileset_id)),selected="Joule_03_24_2020"),
        plotOutput("predicted_time_plot"),
      leafletOutput("sample_prediction_map")))
    })
}

