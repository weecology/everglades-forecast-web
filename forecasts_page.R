forecasts_page <- function(df) {
  renderUI({
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "forecast_origin",
          "Forecast Origin",
          min      = 2000,
          max      = 2016,
          value = 2000, sep = ""), width = 3),
      mainPanel(
        textOutput("greg_title"),
        imageOutput("greg_Image"),
        textOutput("wost_title"),
        imageOutput("wost_Image"),
        textOutput("whib_title"),
        imageOutput("whib_Image"),
        textOutput("pred_obs_title"),
        imageOutput("pred_obs_Image")
      )

    )
  })
}
