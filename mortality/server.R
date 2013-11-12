library(shiny)
library(rjson)

library(rCharts)

source('~/R/extralife/mortality/plotMort.R')
source('~/R/extralife/mortality/mortality_rCharts.R')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  


  sid <- isolate(sub('^.*sid=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE))
  
  #load data
  #dataurl <- paste("http://thetava.com/shiny-data/people?sid=",sid,sep='') 
  dataurl <- paste("http://cloud.thetaris.com/shiny-data/people?sid=cpl0siilKeGZysYCrIxKa-ZZA6fdWNfjlgK1yoTX_hs",sep="")
  
  data <- data.frame(fromJSON(file=dataurl))    
  
  
  output$mortalityPlot <- renderPlot({

    
    # plot mortality report
    plotFamilyMort(data)
  })
  
  output$demographyPlot <- renderChart({
    # create fake plot as a placeholder    
    hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
    n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart", transitionDuration = "0")
    
    # link with to HTML page
    n1$addParams(dom = 'demographyPlot')      
    
    # show how to use input
    n1$yAxis(tickFormat =  sprintf("#!function(d) {return (d/1000000).toFixed(2) + ' Mio (%i)';}!#",input$year))
    
    
    
    return(n1)
  })
  
  output$myChart <- renderChart({
    

    ############################################################
    n1 <- lebensphasenChart(data)

    ############################################################
    return(n1)
   
  })
})