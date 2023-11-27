current_nest_page <- function(df) {
  renderUI({
    fluidPage(
      auth_ui("auth"),
      verbatimTextOutput("res_auth")
    )
  })
  }
