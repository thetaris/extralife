library(shiny)
library(rjson)

library(rCharts)

source('../common/getELTYPE.R')
source('../common/readDGSData.R')

source('mortality_rCharts.R')
source('plotMort.R')
source('life_phases_rCharts.R')
source('populationForecastrChartsPlot.R')


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  dataObj = isolate(DGSData(session=session))
  #dataObj = isolate(DGSData(file="../test/testdata2.json"))

  name <- dataObj$get("title", type=ELTYPE$Meine.Familie._)
  birthDay <- dataObj$get("person.geburtsdatum", type=ELTYPE$Meine.Familie._)
  sex <- dataObj$get("person.geschlecht", type=ELTYPE$Meine.Familie._)
      
  # convert data to fit other algorithms
  birthDay[birthDay==""] = NA
  birthYear = sapply(birthDay, function(x) as.numeric(format(as.Date(x), "%Y")))
  birthYear[is.na(birthYear)] = 0
  
  sex[sex==""]    <- 0
  sex[is.na(sex)] <- 0
  
  data = data.frame(name, birthYear, birthDay, sex, row.names=NULL)
  
  # sort by age
  data = data[order(-data$birthYear),]

  # read data for population forecast
  PopulationForecastDE<<-read.delim(file = "../mortality/data/PopulationForecastDE.txt", header = FALSE, )
  
  
  output$mortalityPlot <- renderPlot({    
    # plot mortality report
    plotFamilyMort(data)
  })
  
  
  
  output$mortalityPlotRCharts<- renderChart({  
   tmpData = data[data$name==input$dataName,]
    
   n2 <- mortalityHistogram(as.Date(tmpData$birthDay), tmpData$sex, tmpData$name)        
   #n2 = plotMortality_rCharts(data[data$name==input$dataName,])
         
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
    
#   n1 <- plotPopulationForecast_rCharts(input$year,name[1:2], birthYear[1:2], tmp_sex[1:2])
    # link with to HTML page
#    n1$addParams(dom = 'demographyPlot')      
    
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