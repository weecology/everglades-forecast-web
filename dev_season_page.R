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
        p("The current session data is being processed and is not publicly available.
        Please log in to access the current season's data."),

        # Wrap the form elements in tags$form to enable Enter key submission
        tags$form(
          id = "login-form",
          textInput("dev_username", "Username"),
          passwordInput("dev_password", "Password"),
          actionButton("dev_login", "Login", class = "btn-primary"),

          # Add JavaScript to handle Enter key press
          tags$script(HTML("
            $(document).ready(function() {
              $('#dev_username, #dev_password').keypress(function(e) {
                if(e.which == 13) { // Enter key code
                  $('#dev_login').click();
                }
              });
            });
          "))
        ),
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
              choices = c("All", unique(colonies$site)),
              selected = "Joule"
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
