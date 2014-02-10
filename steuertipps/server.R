library(shiny)
library(rCharts)

require(ggplot2)
require(gridSVG)
require(XML)


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  set.seed(955)
  # Make some noisily increasing data
  dat <- data.frame(cond = rep(c("A", "B"), each=10),
                    xvar = 1:20 + rnorm(20,sd=3),
                    yvar = 1:20 + rnorm(20,sd=3))
  # cond         xvar         yvar
  #    A -4.252354091  3.473157275
  #    A  1.702317971  0.005939612
  #   ... 
  #    B 17.793359218 19.718587761
  #    B 19.319909163 19.647899863
  tempsvg <- tempfile(fileext=".svg")
  svg(tempsvg, width=4, height=4)
  g4 <- ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_smooth() +  #we'll see why order is important
    geom_point(shape=19, aes(color = cond), size=5) 
  print(g4)
  #export to R object
  #grid.export deprecates the older gridToSVG
  #g4.svg <- grid.export(name = NULL, addClasses=F, progress=T)
    
  
  dev.off()
  svgoutput <- readLines(tempsvg, n=-1)#
  file.remove(tempsvg)
  # The following is done on change of input
  output$SVGTest <- renderUI({
    #tags$div(HTML(saveXML(g4.svg$svg)))
    tags$div(HTML(svgoutput))
  })
  
  
  
})