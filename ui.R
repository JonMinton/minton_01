
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

require(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Choropleth and dissimilarity measures"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("vignette",
                  "Select Vignette:",
                  choices=c(
                    "People of working age"="people",
                    "Council houses"= "council_houses"
                  )
                ) 
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("output")
    )
  )
))

