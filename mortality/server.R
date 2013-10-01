library(shiny)
library(rjson)

library(rCharts)

source('plotMort.R')
source('mortality_rCharts.R')

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

    #print(data)

    # read data and hold in session
    
    # grab the sessionId
    sid <- sub('^.*sid=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE)
    
    #load data
    dataurl <- paste("http://thetava.com/shiny-data/people?sid=",sid,sep='')  
    #dataurl <- "http://thetava.com/shiny-data/people?sid=7dB4QjojQsbp4lyilWxa0vXSk6C2U2cf_sTIbFDuaoY"
    
    data <- data.frame(fromJSON(file=dataurl))    
    
    # plot mortality report
    plotFamilyMort(data)
  })
  
  output$myChart <- renderChart({
    
    # read data and hold in session
    
    # grab the sessionId
    sid <- sub('^.*sid=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE)
    
    #load data
    dataurl <- paste("http://thetava.com/shiny-data/people?sid=",sid,sep='')  
    #dataurl <- "http://thetava.com/shiny-data/people?sid=7dB4QjojQsbp4lyilWxa0vXSk6C2U2cf_sTIbFDuaoY"
    
    data <- data.frame(fromJSON(file=dataurl))
    ############################################################
    n1 <- lebensphasenChart(data)

    ############################################################
    return(n1)
   
  })
})