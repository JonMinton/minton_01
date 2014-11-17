
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("do_prereqs.r")
require(shiny)


shinyServer(function(input, output) {
  this_choice <- reactive({input$vignette})

  output$output <- renderPlot({
  
    if(this_choice()=="people"){
      print("The choice is people")
      g1 <- ggplot(pop_joined)
      g2 <- g1 + geom_polygon(aes(x=long, y=lat, fill=proportion, group=id)) + coord_equal()
      g3 <- g2 + theme_clean() + scale_fill_gradient(low="white", high="red", limits=c(0,1))      
      print(g3)
    }
    if (this_choice()=="council_houses"){
      print("The choice is council_houses")
      g1 <- ggplot(house_joined)
      g2 <- g1 + geom_polygon(aes(x=long, y=lat, fill=proportion, group=id)) + coord_equal()
      g3 <- g2 + theme_clean() + scale_fill_gradient(low="white", high="blue", limits=c(0,1))
      print(g3)
    }
    

  })

})
