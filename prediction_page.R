predicted_page<-function(df){
  unique(df$tiles)
  renderUI({
      sidebarLayout(sidebarPanel(leafletOutput("map", height=300), width = 2),
          
      mainPanel(
        splitLayout(
        selectInput("prediction_site","Site",choices = sort(unique(df$site)),selected="Joule"),
        selectInput("prediction_species","Species",choices = sort(unique(df$label)), multiple = TRUE),
        ),
        plotOutput("predicted_time_plot"),
        selectInput("prediction_tileset","Event",choices = sort(unique(df$tileset_id)),selected="Joule_03_24_2020"),
      leafletOutput("sample_prediction_map")))
    })
}

