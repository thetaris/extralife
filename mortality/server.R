library(shiny)
library(rjson)

library(rCharts)

source('plotMort.R')
source('plotMort_rCharts.R')
source('mortality_rCharts.R')
source('populationForecastrChartsPlot.R')
source('../common/readDGSData.R')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  sid <- isolate(sub('^.*sid=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE))
  
  
  data <- readDGSData(file = "../test/testdata.json", requestedFields = c("title","person.geburtsdatum", "person.geschlecht"))
  
  # select only people with birthday
  data <- data.frame(data[!sapply(data[,2], is.null),])
  
  # convert data to fit other algorithms
  name = unlist(data$title)
  birthYear = sapply(data$person.geburtsdatum, function(x) as.numeric(format(as.Date(x), "%Y")))
  sex = c(numeric(length(data)))
  sex[data$person.geschlecht == "mann"] <- 1
  sex[data$person.geschlecht == "frau"] <- 2
  sex[is.na(data$person.geschlecht)] <- 0
  data = data.frame(name, birthYear, sex)

  # read data for population forecast
  PopulationForecastDE<-read.delim(file = "../mortality/data/PopulationForecastDE.txt", header = FALSE, )
  
  
  output$mortalityPlot <- renderPlot({    
    # plot mortality report
    plotFamilyMort(data)
  })
  
  
  
  output$mortalityPlotRCharts<- renderChart({  
        
    n2 = plotMortality_rCharts(data[data$name==input$dataName,])
    
    # link with HTML page
    n2$addParams(dom = 'mortalityPlotRCharts')      
    
    
    return(n2)      
  })
  
  output$dataNames <-renderUI({
    selectInput("dataName", "Person", data[,1])
  })
  
  output$demographyPlot <- renderChart({
    tmp_sex <- sex;
    tmp_sex[tmp_sex==1] <-"m"  
    tmp_sex[tmp_sex==2] <-"w"  
    
    n1 <- plotPopulationForecast_rCharts(input$year,name[1:2], birthYear[1:2], tmp_sex[1:2])
    # link with to HTML page
    n1$addParams(dom = 'demographyPlot')      
    
    # show how to use input
    #n1$yAxis(tickFormat =  sprintf("#!function(d) {return (d/1000000).toFixed(2) + ' Mio (%i)';}!#",input$year))
    
    
    
    return(n1)
  })
  
  output$myChart <- renderChart({
    

    ############################################################
    n1 <- lebensphasenChart(data)

    ############################################################
    return(n1)
   
  })
})