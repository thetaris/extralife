library(shiny)
library(rjson)

library(rCharts)

source("../common/INIT.R", chdir=T, encoding="UTF-8")

source('prepareData.R')
source('mortality_rCharts.R')
source('life_phases_rCharts.R')
source('demography_rCharts.R')


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  ##################################
  # read data and prepare for plot #
  # only once per session          #
  ##################################
  dataObj = isolate(DGSData(session=session))
  #dataObj = isolate(DGSData(file="../test/data/test_simpson.json"))
  
  data = prepareDataFamily(dataObj)
  
  PopulationForecastDE<<-prepareDataDemography()
  
  densities <<- getDensitiesFromFile()
  
  #######################################
  # functions for tab creation          #
  # called after each click on shiny ui #
  #######################################
  output$dataNames <-renderUI({
    # create fields for person selector
    selectInput("dataName", "Person", data[,1])
  })

  output$mortality<- renderChart({  
    # create subset of data with selected person
    tmpData = data[data$name==input$dataName,]    
    n2 <- mortalityHistogram(as.Date(tmpData$birthDay), tmpData$sex, tmpData$name)           
    
    return(n2)      
  })  
  
  output$demography <- renderChart({    
    n1 <- demographyChart(input$year,data$name, data$birthYear, gender_in = data$sex)
    # does not work yet in shiny, rCharts has a bug there
    #n1 <- demographyChartControlled()
    
    return(n1)
  })
  
  output$life_phases <- renderChart({
    n1 <- lebensphasenChart(data)
    return(n1)
    
  })
  
  output$dataUsed = renderDataTable({
    log <- dataObj$getLog()
    log <- log[order(log$node_id),c("title","field","value", "estimatedFlag")]
    return(log)
  }, options = list(aLengthMenu = c(5, 50, 250), iDisplayLength = 50))
  
})