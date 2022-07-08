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
  
  map_filter<-reactive({
    #filter based on selection
    if(is.null(input$prediction_site)){
      return(colonies)
    }
    
    map_data <- colonies %>% filter(site==input$prediction_site)
    return(map_data)
  })
  
  ##Prediction panel##
  prediction_filter<-reactive({
    if(is.null(input$mapbox_date)){
      mapbox_date = "2020-02-24"
    } else{
      mapbox_date = input$mapbox_date
    }
    
    #filter based on selection
    print(paste("mapbox date is:", mapbox_date))
    print(paste("selected site is:", site_name_filter()))
    
    selected_species = species_name_filter()
    if(selected_species == "All"){
      to_plot <- df %>% filter(site==site_name_filter(), event==mapbox_date) 
    } else{
      to_plot <- df %>% filter(site==site_name_filter(), event==mapbox_date, label==species_name_filter()) 
    }
    return(to_plot)
  })
  
  site_name_filter<-reactive({
    return(input$prediction_site)
  })
  
  species_name_filter<-reactive({
    if(input$prediction_species=="All"){
      return("All")
    } else{
      return(input$prediction_species)
    }
  })
  
  output$date_slider = renderUI({
    print(paste("selected_size is:",site_name_filter()))
    selected_df <- df %>%
      filter(site==site_name_filter())
    available_dates<-sort(unique(selected_df$event))
    sliderTextInput(inputId = "mapbox_date","Select Date",choices=available_dates)
  })
  
  output$predicted_time_plot<-renderPlot(time_predictions(df, site_name_filter(),species=species_name_filter(), selected_event=input$mapbox_date))
  output$sample_prediction_map<-renderLeaflet(plot_predictions(df=prediction_filter(),MAPBOX_ACCESS_TOKEN))
})
