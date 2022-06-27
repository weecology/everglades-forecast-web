#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
library(shinyWidgets)
library(htmltools)
library(sf)
library(stringr)

#Source page UIs
source("about_page.R")
source("prediction_page.R")
source("functions.R")
source("load_data.R")

shinyServer(function(input, output, session) {
  output$zooniverse_anotation<-renderPlot(zooniverse_complete())

  #Setmapbox key
  readRenviron("source_token.txt")
  MAPBOX_ACCESS_TOKEN=Sys.getenv("MAPBOX_ACCESS_TOKEN")

  #Create pages
  output$about<-about_page()
  output$predicted<-predicted_page(df)

  ####Sidebar Map###
  output$map <- create_map(colonies)
  
  observe({
    new_map_data = map_filter()
    leafletProxy("map", data=new_map_data) %>% clearMarkers() %>% addMarkers(popup=~site)
  })
  
  ##Prediction page##
  prediction_filter<-reactive({
    #filter based on selection
    to_plot <- df %>% filter(tileset_id==input$prediction_tileset) 
    return(to_plot)
  })
  
  site_name_filter<-reactive({
    return(input$prediction_site)
  })
  
  map_filter<-reactive({
    #filter based on selection
    if(is.null(input$prediction_site)){
      return(colonies)
    }
    map_data <- colonies %>% filter(site==input$prediction_site)
    return(map_data)
  })
  
  output$predicted_time_plot<-renderPlot(time_predictions(df, site_name_filter(),species=input$prediction_species))
  output$sample_prediction_map<-renderLeaflet(plot_predictions(df=prediction_filter(),MAPBOX_ACCESS_TOKEN))
})
