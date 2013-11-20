library(shiny)
library(rjson)

library(rCharts)

source('plotMort.R')
source('plotMort_rCharts.R')
source('mortality_rCharts.R')
source('../common/readDGSData.R')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  


  sid <- isolate(sub('^.*sid=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE))
  
  #load data
  #dataurl <- paste("http://thetava.com/shiny-data/people?sid=",sid,sep='') 
#  dataurl <- paste("http://cloud.thetaris.com/shiny-data/people?sid=cpl0siilKeGZysYCrIxKa-ZZA6fdWNfjlgK1yoTX_hs",sep="")
  
#  data <- data.frame(fromJSON(file=dataurl))    
  
  data <- readDGSData(sid = "cpl0siilKeGZysYCrIxKa-ZZA6fdWNfjlgK1yoTX_hs", requestedFields = c("title","person.geburtsdatum", "person.geschlecht"))
  
  # select only people with birthday
  data <- data[is.na(data[,2])==FALSE,]
  
  # convert data to fit other algorithms
  name = data$title
  birthYear = as.numeric(format(as.Date(data$person.geburtsdatum), "%Y"))
  sex = c(numeric(length(data)))
  sex[data$person.geschlecht == "mann"] <- 1
  sex[data$person.geschlecht == "frau"] <- 2
  sex[is.na(data$person.geschlecht)] <- 0
  data = data.frame(name, birthYear, sex)
  
  output$mortalityPlot <- renderPlot({    
    # plot mortality report
    plotFamilyMort(data)
  })
  
  output$mortalityPlotRCharts<- renderChart({
    familyID <-data.frame(
      name = c("Smino","Katie","Anette","David","EmptyID"),
      birthYear = c(1920,1976,1984,2012,0),
      # 1 for male, 2 for female, 0 for na
      sex = c(1,2,2,1,0)
    )
    
        
    n2 = plotMortality_rCharts(data[data$name==input$dataName,])
    #n2 = plotMortality_rCharts(familyID[familyID$name=="Anette",])
    
    # link with HTML page
    n2$addParams(dom = 'mortalityPlotRCharts')      
    
    
    return(n2)      
  })
  
  output$dataNames <-renderUI({
    selectInput("dataName", "Person", data[,1])
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