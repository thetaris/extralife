library(shiny)
library(rCharts)

source("../common/INIT.R", chdir=T, encoding="UTF-8")


source('balance.R')
source('CashFlowAndBalanceSheetStackBarPlot.R', encoding="UTF-8")
source('CashFlowAndBalanceSheetLinePlot1.R')
source('gauge.R')

source('GenReDisabilityBarPlot_rCharts.R')


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  
  # get data from JSON service
  dataObj = isolate(DGSData(session=session))
  #dataObj = isolate(DGSData(file="../test/data/test_simpson.json"))

  Geburtsdatum <- tryCatch(as.Date(dataObj$get(requestedField=ELFIELD$person.geburtsdatum, type=ELTYPE$Ich))
                          , error = function(e) as.Date('1994-01-01')
  )
  
  gender <- tryCatch(dataObj$get(requestedField=ELFIELD$person.geschlecht, type=ELTYPE$Ich)
                     , error = function(e) "frau"
  )
  
  age = max(floor(as.numeric(Sys.Date()- Geburtsdatum, units="days")/365.24),0)
  gender[gender=="mann"]="m"
  gender[gender=="frau"]="f"
  profession_group =  dataObj$get(requestedField=ELFIELD$arbeitsvertrag.beruf, type=ELTYPE$Arbeitsvertrag)
  
  # set default value
  if (is.null(profession_group)){
    profession_group = ELENUM$arbeitsvertrag.beruf$Buerotaetigkeit$value 
  }
  profession_group[is.na(profession_group)] = ELENUM$arbeitsvertrag.beruf$Buerotaetigkeit$value
  
  if (length(profession_group)>1){
    profession_group = max(profession_group)
  }

  # create fields for person selector
  profs <- unlist(lapply(ELENUM$arbeitsvertrag.beruf, function(x) x$key), use.names=F)
  values <- unlist(lapply(ELENUM$arbeitsvertrag.beruf, function(x) x$value), use.names=F)  
  
  output$dataProf <-renderUI({
    res = list()
    selected <- profs[values == profession_group & !is.na(values)]
      
    res1 = selectInput("dataAge", "", c(23:65), selected=age )
    if (any(gender=="m")){
      res2 = selectInput("dataGender", "", c("Mann", "Frau"), selected="Mann" )  
    }else{
      res2 = selectInput("dataGender", "", c("Mann", "Frau"), selected="Frau" ) 
    }
    
    res3 = selectInput("dataProf", "", profs, selected=selected ) 
    
    res = tags$table(tags$tr(tags$td(res1), tags$td(res2), tags$td(res3)))
    
    return(res)
  })  
  
  output$risk <- renderChart2({
    # dirty hack due to encoding problems (HTML and UTF-8), compare only first character
    # should be: 
    # selected <- values[profs == input$dataProf & !is.na(values)]
    selectedProf <- values[substr(profs,1,1) == substr(input$dataProf,1,1) & !is.na(values)]

    if (is.null(selectedProf) | length(selectedProf)==0){
      selectedProf = profession_group
    }
    
    selectedAge <- as.numeric(input$dataAge)
    if (any(input$dataGender=="Mann")){
      selectedGender <- "m"  
    }else{
      selectedGender <- "f"  
    }    
        
    disabilityID <-data.frame(age = selectedAge, gender= selectedGender, profession_group=selectedProf)          
    p <- plotDisability_rCharts(disabilityID[1,])
   # p$addParams(dom = 'risk')
  
    
     p$set(width = 700)    
     p$set(height = 300)    
    return(p)
  })  
  
  output$overview <- renderChart2({
    
    Name_frame = c(character())
    Einkommen_frame= c(character())
    Wert_frame= c(numeric())
    
    Names = c(character())
    Names[1] = 'gesund'
    Names[2] = 'berufsunfähig'
    
    Einkommen = dataObj$get(requestedField=ELFIELD$i.einkommen.monatlich)
    Einkommen[is.na(Einkommen)]=0
    Einkommen = sum(Einkommen)
    
    Arbeitsvertrag = c(Einkommen, Einkommen, 0)
    Erwerbsminderungsrente = c(Einkommen/3, Einkommen/3)
    
    #Geburtsdatum <- as.Date(dataObj$get(requestedField=ELFIELD$person.geburtsdatum, type=ELTYPE$Ich))
    
    EinkommenDauer = as.numeric(rentenEintrittsDatum(Geburtsdatum) - Sys.Date(), units="days")/365.24
    
    for(i in 1:length(Names)){
      
      if (i==1){
        Name_frame[(i-1)*3+1] = Names[i]
        Einkommen_frame[(i-1)*3+1] = "Arbeitsvertrag"
        Wert_frame[(i-1)*3+1] = Arbeitsvertrag[i] * 12 * EinkommenDauer
        
        Name_frame[(i-1)*3+2] = Names[i]
        Einkommen_frame[(i-1)*3+2] = "Krankentagegeld"
        Wert_frame[(i-1)*3+2] = 0
        
        Name_frame[(i-1)*3+3] = Names[i]
        Einkommen_frame[(i-1)*3+3] = "Erwerbsminderungsrente"
        Wert_frame[(i-1)*3+3] = 0
        
      } else {
        Name_frame[(i-1)*3+1] = Names[i]
        Einkommen_frame[(i-1)*3+1] = "Arbeitsvertrag"
        Wert_frame[(i-1)*3+1] = 0
        
        Name_frame[(i-1)*3+2] = Names[i]
        Einkommen_frame[(i-1)*3+2] = "Krankentagegeld"
        Wert_frame[(i-1)*3+2] = Arbeitsvertrag[i] * 0.8 * 1.5 * 12
        
        Name_frame[(i-1)*3+3] = Names[i]
        Einkommen_frame[(i-1)*3+3] = "Erwerbsminderungsrente"
        Wert_frame[(i-1)*3+3] = Erwerbsminderungsrente[i] * (EinkommenDauer-3) * 12
      }
    }
    
    
    
    phase = data.frame(Name_frame, Einkommen_frame, Wert_frame)
    colnames(phase) = c('Name', 'Lebensabschnitt', 'Dauer')
    
    n1 <- nPlot(Dauer ~ Name, data = phase, group = 'Lebensabschnitt', type = 'multiBarChart')
    n1$yAxis(tickFormat = "#!function(d) {return (d/1000000).toFixed(2) + ' Mio';}!#")
    n1$set(width = 450)
    n1$set(height = 300)    
    n1$chart(stacked = 'true')
    n1$chart(showControls = F)         
    return(n1)
    
    ##################################################################################################
    
  })
  

  
})