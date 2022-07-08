predicted_page<-function(df){
  renderUI({
      sidebarLayout(
        sidebarPanel(
          selectInput("prediction_species","Species",choices = c("All",sort(unique(df$label))), multiple = TRUE, selected = "All"),
          selectInput("prediction_site","Site",choices = c("All",sort(unique(df$site))),selected="Joule"),
          leafletOutput("map", height=300),
          uiOutput("date_slider"), 
          width = 3),
      mainPanel(
      plotOutput("predicted_time_plot"),
      leafletOutput("sample_prediction_map")))
    })
}

