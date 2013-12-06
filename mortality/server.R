library(shiny)
library(rjson)

library(rCharts)

source('../common/getELTYPE.R')
source('../common/readDGSData.R')

source('prepareData.R')
source('mortality_rCharts.R')
#source('plotMort.R')
source('life_phases_rCharts.R')
source('demography_rCharts.R')


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  dataObj = isolate(DGSData(session=session))
  #dataObj = isolate(DGSData(file="../test/testdata2.json"))
  
  data = prepareDataFamily(dataObj)
  PopulationForecastDE<<-prepareDataDemography()

  
  output$mortalityPlot <- renderPlot({    
    # plot mortality report
    plotFamilyMort(data)
  })
  
  
  output$mortalityPlotRCharts<- renderChart({  
   tmpData = data[data$name==input$dataName,]
    
   n2 <- mortalityHistogram(as.Date(tmpData$birthDay), tmpData$sex, tmpData$name)           
         
    # link with HTML page
   n2$addParams(dom = 'mortalityPlotRCharts')      
        
   return(n2)      
  })
  
  output$dataNames <-renderUI({
    selectInput("dataName", "Person", data[,1])
  })
  
  output$demographyPlot <- renderChart({
    tmp_sex <- sex;
    tmp_sex[tmp_sex=="mann"] <-"m"  
    tmp_sex[tmp_sex=="frau"] <-"w"  
    
    n1 <- demographyChart(input$year,name, birthYear, gender_in = tmp_sex)
    # does not work yet in shiny, rCharts has a bug there
    #n1 <- demographyChartControlled()
    
    # link with to HTML page
      
                
    return(n1)
  })
  
  output$myChart <- renderChart({
    

    ############################################################
    n1 <- lebensphasenChart(data)

    ############################################################
    return(n1)
   
  })
})