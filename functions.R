#functions
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(gridExtra)
library(stringr)
library(htmltools)
library(tidyr)

#Site map
create_map<-function(colonies){
  m <- leaflet(data=colonies) %>% addTiles() %>% addMarkers(popup =~site) 
  return(renderLeaflet(m))
}

#Load data
load_classifications<-function(){
  raw_data<-read_sf("data/everglades-watch-classifications.shp")
  st_crs(raw_data)<-32617
  return(raw_data)
}

#Filter classification by spatial overlap

check_events<-function(x){
  if(str_detect(x,"_")){
    return(str_match(x,"(\\w+)_")[,2])
  }else{
    return(x)
  }
}
filter_annotations<-function(raw_data){
  selected_ids<-unique(raw_data$selected_i)
  
  #Majority rule for labels
  majority_rule<-raw_data %>%
                 data.frame() %>% # Converting to a non-spatial data frame improves speed 100-200x
                 group_by(selected_i, label) %>%
                 summarize(n=n()) %>%
                 arrange(desc(n)) %>%
                 slice(1) %>%
                 as.data.frame() %>%
                 mutate(majority_class=label) %>%
                 dplyr::select(selected_i,majority_class)
  
  selected_boxes<-raw_data %>% filter(selected_i %in% selected_ids) %>% inner_join(majority_rule) %>% filter(!is.na(event))
  
  #!!Temp hotfix!!! until events are seperated from dates
  #selected_boxes$event<-sapply(selected_boxes$event,check_events)
  selected_boxes$event[selected_boxes$event %in% "03112020"]<-gsub(x=selected_boxes$event[selected_boxes$event %in% "03112020"],pattern="03112020",replacement="03_11_2020")
  selected_boxes$event<-as.Date(selected_boxes$event,"%m_%d_%Y")
  selected_boxes$tileset_id<-construct_id(selected_boxes$site,selected_boxes$event)
  
  #get unique boxes among observers
  
  return(selected_boxes)
}

plot_annotations<-function(selected_boxes, MAPBOX_ACCESS_TOKEN){
  pal <- colorFactor(
    palette = 'Dark2',
    domain = selected_boxes$species
  )
  
  selected_centroids<-st_transform(selected_boxes,4326)
  
  #Create mapbox tileset
  mapbox_tileset<-unique(selected_centroids$tileset_id)
  mapbox_tileset<-paste("bweinstein.",mapbox_tileset,sep="")
  
  m<-leaflet(data=selected_centroids) %>%
    addProviderTiles("MapBox", options = providerTileOptions(id = mapbox_tileset, minZoom = 8, maxNativeZoom=24, maxZoom = 24, accessToken = MAPBOX_ACCESS_TOKEN)) %>%
    addCircles(stroke = T,color=~pal(species),fillOpacity = 0.1,radius = 0.25,popup = ~htmlEscape(label))
  return(m)
}

plot_predictions<-function(df, MAPBOX_ACCESS_TOKEN){
  mapbox_tileset<-unique(df$tileset_id)
  mapbox_tileset<-paste("bweinstein.",mapbox_tileset,sep="")
  
  m<-leaflet(data=df) %>% 
    addProviderTiles("MapBox", options = providerTileOptions(id = mapbox_tileset, minZoom = 8, maxNativeZoom=24, maxZoom = 24, accessToken = MAPBOX_ACCESS_TOKEN)) %>%
    addCircles(stroke = T,fillOpacity = 0.1,radius = 0.25,popup = ~htmlEscape(paste(label,round(score,2),sep=":")))
  return(m)
}

time_predictions<-function(df, select_site){
  g <- df %>% filter(site==select_site) %>% group_by(site,event, year) %>% summarize(n=n())
  ggplot(g,aes(x=event,y=n)) + geom_point() + geom_line(aes(color=year)) + labs(y="Predicted Birds",x="Date") + theme(text = element_text(size=20))
}

species_colors <- colorFactor(palette = c("yellow", "blue",
                                          "#ff007f", "brown",
                                          "purple", "white"),
                              domain = c("Great Egret", "Great Blue Heron",
                                         "Roseate Spoonbill", "Wood Stork",
                                          "Snowy Egret", "White Ibis"),
                              ordered=TRUE)

#Construct mapbox url
construct_id<-function(site,event){
  event_formatted<-format(event, "%m_%d_%Y")
  tileset_id <- paste(site,"_",event_formatted,sep="")
  return(tileset_id)
}