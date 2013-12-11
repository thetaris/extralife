library(shiny)
library(rCharts)
require(png)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  
  
  
  # The following is done on change of input

  
  output$welcomeImg <- renderPlot({
    img <- readPNG("welcome.png")
    plot.new()
    rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4], interpolate=TRUE)              
  }, height=650, width=651 )
  
  
  
})