#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
library(shinyWidgets)
library(htmltools)
library(sf)
library(stringr)

#Source page UIs
source("landing_page.R")
source("time_page.R")
source("about_page.R")
source("prediction_page.R")
source("predicted_nest_page.R")
source("functions.R")
source("load_data.R")

shinyServer(function(input, output, session) {
  output$zooniverse_anotation<-renderPlot(zooniverse_complete())

  #Setmapbox key
  readRenviron("source_token.txt")
  MAPBOX_ACCESS_TOKEN=Sys.getenv("MAPBOX_ACCESS_TOKEN")

  #Create pages
  output$landing<-landing_page(selected_boxes)
  output$time<-time_page(selected_boxes)
  output$about<-about_page()
  output$predicted<-predicted_page(df)
  output$predicted_nests<-predicted_nest_page(nestdf)
  
  ####Landing page###
  output$map <- create_map(colonies)
  landing_filter<-reactive({
    #filter based on selection
    if(is.null(input$landing_site)){return(selected_boxes)}
    if(!"All" %in% input$landing_site){
      to_plot <- selected_boxes %>% filter(site %in% input$landing_site) 
    }
    else{
      to_plot<-selected_boxes
    }
    return(to_plot)
  }) 
  
  landing_map_select<-reactive({
    #filter based on selection
    if(is.null(input$landing_site)){return(colonies)}
    if(!"All" %in% input$landing_site){
      to_plot <- colonies %>% filter(colony %in% input$landing_site) 
    } else{
      to_plot<-colonies
    }
    return(to_plot)
  }) 
  
  observe({
    leafletProxy("map", data=landing_map_select()) %>% clearMarkers() %>% addMarkers(popup=~colony)
  })
  
  observe({
    output$site_totals_plot<-renderPlot(site_totals(selected_boxes=landing_filter()))
  })

  output$summary <- renderText(paste("There have been",nrow(raw_data),"classifications on",length(unique(raw_data$subject_id)),"non-empty frames by", length(unique(raw_data$user_name)),"users at",length(unique(raw_data$site)),"sites"))
  output$totals_plot<-renderPlot(totals_plot(selected_boxes))
  
  # View Zooniverse annotations
  time_series_filter<-reactive({
    #filter based on selection
    to_plot <- selected_boxes %>% filter(site %in% input$timeseries_site, species %in% input$timeseries_species,behavior %in% input$timeseries_behavior) 
    return(to_plot)
  }) 
  
  observe({
    output$site_phenology_plot<-renderPlot(site_phenology(selected_boxes=time_series_filter()))
  })
  
  ###Species page###
  output$label_heatmap<-renderPlot(behavior_heatmap(selected_boxes))
  
  colony_filter<-reactive({
  #filter based on selection
    to_plot <- selected_boxes %>% filter(tileset_id==input$selected_image) 
    return(to_plot)
  })
  
  observe({
    output$colony_map<-renderLeaflet(plot_annotations(selected_boxes =colony_filter(),MAPBOX_ACCESS_TOKEN))
  })
  
  ##Prediction page##
  prediction_filter<-reactive({
    #filter based on selection
    to_plot <- df %>% filter(tileset_id==input$prediction_tileset) 
    return(to_plot)
  })
  
  output$predicted_time_plot<-renderPlot(time_predictions(df))
  output$sample_prediction_map<-renderLeaflet(plot_predictions(df=prediction_filter(),MAPBOX_ACCESS_TOKEN))
  output$Zooniverse_Predicted_Table<-renderTable(compare_counts(df, selected_boxes))
  
  ###Nest Page###
  nest_filter<-reactive({
    #filter based on selection
    to_plot <- nestdf %>% filter(Site==input$nest_site, Year==input$nest_year)
    return(to_plot)
  })
  output$nest_summary_table <- renderTable(nest_summary_table(nestdf, min_detections))
  output$nest_history_plot <- renderPlot(nest_history(nest_filter()))

  #Reactive UI selector for years
  output$nest_year_selector = renderUI({
    selected_site <- as.character(input$nest_site)
    selected_df <- nestdf %>% filter(Site==selected_site)
    available_years<-sort(unique(selected_df$Year))
    selectInput(inputId = "nest_year","Year",choices=available_years)
  })

  #Reactive UI slider for dates
  output$nest_date_slider = renderUI({
    selected_site <- as.character(input$nest_site)
    selected_year <- input$nest_year
    selected_df <- nestdf %>%
      filter(Site==selected_site, Year==selected_year) %>%
      filter(Date < "2021-05-05") # Filter out dates where field nests are flagged to avoid indicating which field nests are real
    available_dates<-sort(unique(selected_df$Date))
    sliderTextInput(inputId = "nest_date","Select Date",choices=available_dates)
  })

  #Reactive UI slider for species
  output$species_selector <- renderUI({
    species = c("Great Egret", "Great Blue Heron",
                "Roseate Spoonbill", "Wood Stork",
                "Snowy Egret", "White Ibis")
    pickerInput(inputId = "species",
                label = "Species",
                multiple = TRUE,
                choices = species,
                selected = species,
                options = list(`actions-box` = TRUE))
  })

  #Reactive UI selector for Nest IDs
  output$nest_id_selector = renderUI({
    selected_site <- as.character(input$nest_site)
    selected_year <- input$nest_year
    selected_df <- nestdf %>% filter(Site==selected_site, Year==selected_year)
    available_nests<-sort(unique(selected_df$target_ind))
    pickerInput(inputId = "nest_ids",
                label = "Nest IDs",
                multiple = TRUE,
                choices = available_nests,
                options = list(`actions-box` = TRUE))
  })

  #Reactive UI selector for Bird IDs
  output$bird_id_selector <- renderUI({
    selected_site <- as.character(input$nest_site)
    selected_year <- input$nest_year
    selected_event <- input$nest_date
    selected_df <- df %>% filter(site==selected_site, year==selected_year, event==input$nest_date)
    available_birds <- sort(unique(selected_df$bird_id))
    pickerInput(inputId = "bird_ids",
                label = "Bird IDs",
                multiple = TRUE,
                choices = available_birds,
                options = list(`actions-box` = TRUE))
  })

  #Reactive UI selector for Field Nest IDs
  output$field_nest_samp_id_selector <- renderUI({
    selected_site <- as.character(input$nest_site)
    selected_year <- input$nest_year
    selected_field_nests <- field_nests %>% filter(site==selected_site, year==selected_year)
    available_nests <- str_sort(unique(selected_field_nests$sample_id), numeric = TRUE)
    pickerInput(inputId = "field_nest_samp_ids",
                label = "Field Nest IDs",
                multiple = TRUE,
                choices = available_nests,
                options = list(`actions-box` = TRUE))
  })

  #Default plot
  output$nest_map<-renderLeaflet(plot_nests(nestdf %>% filter(Site=="Joule") %>% filter(Date==min(Date)),
                                            df %>% filter(site=="Joule") %>% filter(event==min(event)),
                                            MAPBOX_ACCESS_TOKEN))
  
  nest_map_site_filter<-reactive({
    selected_nests<-nestdf %>% filter(Site == input$nest_site)
    return(selected_nests)
  })

  bird_map_site_filter <- reactive({
    selected_birds <- df %>% filter(site == input$nest_site)
    return(selected_birds)
  })

  field_nests_site_filter <- reactive({
    selected_field_nests <- field_nests %>% filter(site == input$nest_site)
    return(selected_field_nests)
  })

  nest_map_date_filter<-reactive({
    selected_nests<-nestdf %>% filter(Date == input$nest_date)
    return(selected_nests)
  })

  bird_map_date_filter<-reactive({
    req(input$nest_date)
    selected_birds <- df %>% filter(event == input$nest_date)
    return(selected_birds)
  })

  observeEvent(input$nest_site,{
    selected_nests<-nest_map_site_filter()
    selected_birds <- bird_map_site_filter()
    selected_field_nests <- field_nests_site_filter()
    min_date <- min(selected_nests$Date)
    output$nest_map<-renderLeaflet(
      plot_nests(
        selected_nests %>% filter(Date==min_date),
        selected_birds %>% filter(event==min_date),
        MAPBOX_ACCESS_TOKEN))
  })

  observeEvent(input$nest_date,{
    selected_nests<-nest_map_date_filter()
    selected_birds <- bird_map_date_filter()
    selected_field_nests <- field_nests_site_filter() %>%
      filter(year ==input$nest_year)
    selected_nests<-selected_nests %>% filter(Site==input$nest_site)
    selected_birds <- selected_birds %>%
                        filter(site == input$nest_site) %>%
                        filter(label %in% input$species)
    mapbox_tileset<-unique(selected_nests$tileset_id)[1]
    selected_nests<-selected_nests %>% filter(target_ind %in% as.numeric(input$nest_ids))
    if (exists("focal_position")){
      update_nests(mapbox_tileset, selected_nests, selected_birds, selected_field_nests, MAPBOX_ACCESS_TOKEN, focal_position)
    } else {
      update_nests(mapbox_tileset, selected_nests, selected_birds, selected_field_nests, MAPBOX_ACCESS_TOKEN)
    }
  })

  observeEvent(input$species,{
    req(input$nest_date)
    selected_nests <- nest_map_date_filter()
    selected_birds <- bird_map_date_filter()
    selected_field_nests <- field_nests_site_filter() %>%
      filter(year == input$nest_year)
    selected_nests <- selected_nests %>% filter(Site==input$nest_site)
    selected_birds <- selected_birds %>%
                        filter(site == input$nest_site) %>%
                        filter(label %in% input$species)
    mapbox_tileset<-unique(selected_nests$tileset_id)[1]
    selected_nests<-selected_nests %>% filter(target_ind %in% as.numeric(input$nest_ids))
    if (exists("focal_position")){
      update_nests(mapbox_tileset, selected_nests, selected_birds, selected_field_nests, MAPBOX_ACCESS_TOKEN, focal_position)
    } else {
      update_nests(mapbox_tileset, selected_nests, selected_birds, selected_field_nests, MAPBOX_ACCESS_TOKEN)
    }
  })

  observeEvent(input$nest_ids,{
    selected_nests<-nest_map_date_filter()
    selected_birds <- bird_map_date_filter()
    selected_field_nests <- field_nests_site_filter() %>%
      filter(year == input$nest_year)
    selected_nests<-selected_nests %>% filter(Site==input$nest_site)
    selected_birds <- selected_birds %>%
                        filter(site == input$nest_site) %>%
                        filter(label %in% input$species)
    mapbox_tileset<-unique(selected_nests$tileset_id)[1]
    selected_nests<-selected_nests %>% filter(target_ind %in% as.numeric(input$nest_ids))
    update_nests(mapbox_tileset, selected_nests, selected_birds, selected_field_nests, MAPBOX_ACCESS_TOKEN)
  })

  observeEvent(input$bird_ids,{
    selected_nests<-nest_map_date_filter() %>%
      filter(Site==input$nest_site) %>%
      filter(target_ind %in% as.numeric(input$nest_ids))
    selected_birds <- bird_map_date_filter() %>%
      filter(site == input$nest_site) %>%
      filter(label %in% input$species)
    selected_field_nests <- field_nests_site_filter() %>%
      filter(year == input$nest_year) %>%
      filter(sample_id %in% as.numeric(input$field_nest_samp_ids))
    mapbox_tileset<-unique(selected_nests$tileset_id)[1]

    if (length(input$bird_ids) == 1){
      focal_bird <- filter(selected_birds, bird_id == input$bird_ids)
      focal_position <<- focal_bird$geometry[[1]]
    } else {
      focal_position <- NULL
    }

    update_nests(mapbox_tileset,
                  selected_nests,
                  selected_birds,
                  selected_field_nests,
                  MAPBOX_ACCESS_TOKEN,
                  focal_position)
  })

  observeEvent(input$field_nest_samp_ids,{
    selected_nests <- nest_map_date_filter() %>%
      filter(Site==input$nest_site)
    mapbox_tileset <- unique(selected_nests$tileset_id)[1]
    selected_nests <- selected_nests %>%
      filter(target_ind %in% as.numeric(input$nest_ids))
    selected_birds <- bird_map_date_filter() %>%
      filter(site == input$nest_site) %>%
      filter(label %in% input$species)
    selected_field_nests <- field_nests_site_filter() %>%
      filter(year == input$nest_year) %>%
      filter(sample_id %in% as.numeric(input$field_nest_samp_ids))

    if (length(input$field_nest_samp_ids) == 1){
      focal_field_nest <- selected_field_nests %>%
        filter(sample_id == input$field_nest_samp_ids)
      focal_position <<- focal_field_nest$geometry[[1]]
    } else {
      focal_position <- NULL
    }

    update_nests(mapbox_tileset,
      selected_nests,
      selected_birds,
      selected_field_nests,
      MAPBOX_ACCESS_TOKEN,
      focal_position)
  })
})
