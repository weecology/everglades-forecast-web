#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
library(shiny)
library(shinythemes)

#Define thumbnail dir
#Source additional pages

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("readable"),
                  #Navbar to each page
                  navbarPage("Everglades Wading Birds",
                             tabPanel("Species Detection",uiOutput('predicted')),
                             tabPanel("About",uiOutput('about'))
                  )))
