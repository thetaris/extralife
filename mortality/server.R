library(shiny)
library(RJSONIO)
source('plotMort.R')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$mortalityPlot <- renderPlot({

    # grab the sessionId
    sid <- sub('^.*sid=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE)
    
    #load data
    dataurl <- paste("http://thetava.com/shiny-data/people?sid=",sid,sep='')
    data <- data.frame(
      fromJSON(
          readChar(dataurl, 10000)))
    #print(data)
    
    # plot mortality report
    plotFamilyMort(data)
  })
})