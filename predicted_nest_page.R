predicted_nest_page<-function(nestdf){
  renderUI({
    fluidPage(
      titlePanel("Predicted Bird & Nest Detections"),
      fluidRow(
        column(width = 2, selectInput("nest_site","Site",choices = unique(nestdf$Site),selected="StartMel")),
        column(width = 2, uiOutput('nest_year_selector'))
      ),
      uiOutput("species_selector"),
      h5("Show:"),
      fluidRow(
        column(width = 1, uiOutput('bird_selector')),
        column(width = 1, uiOutput('nest_selector'))
      ),
      uiOutput('samp_id_selector'),
      uiOutput('date_slider'),
      leafletOutput("nest_map", height=800,width=900)
    )})
}
