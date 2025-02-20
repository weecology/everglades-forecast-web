dev_season_page <- function(df,
                            is_authenticated,
                            current_user,
                            current_year) {
  renderUI({
    req(colonies)
    # Get available species from the data
    available_species <- sort(unique(df$label))

    if (!is_authenticated()) {
      # Show login form when not authenticated
      div(
        style = "max-width: 300px; margin: 0 auto; padding: 20px;",
        h3("Login Required"),
        p("Please log in to view current season data."),
        textInput("dev_username", "Username"),
        passwordInput("dev_password", "Password"),
        actionButton("dev_login", "Login", class = "btn-primary"),
        tags$br(),
        tags$br(),
        textOutput("dev_login_message")
      )
    } else {
      # Show content when authenticated
      fluidRow(column(
        12,
        div(
          style = "float: right;",
          actionButton("dev_logout", "Logout", class = "btn-danger")
        ),
        h3(paste(
          "Current Season Analysis (", current_year(), ")"
        )),
        fluidRow(
          column(
            4,
            selectInput(
              "dev_prediction_site",
              "Select Site",
              choices = c("All", unique(colonies$site))
            ),
            selectInput(
              "dev_prediction_species",
              "Select Species",
              choices = c("All", available_species),
              multiple = TRUE,
              selected = "All"
            ),
            uiOutput("dev_date_slider"),
            leafletOutput("dev_map", height = "400px")
          ),
          column(
            8,
            plotOutput("dev_predicted_time_plot", height = "400px"),
            leafletOutput("dev_sample_prediction_map", height = "400px")
          )
        )
      ))
    }
  })
}
