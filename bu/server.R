library(shiny)
library(rCharts)

source('../common/readDGSData.R')

source('balance.R')
source('CashFlowAndBalanceSheetStackBarPlot.R', encoding="UTF-8")
source('CashFlowAndBalanceSheetLinePlot1.R')
source('gauge.R')

source('GenReDisabilityBarPlot_rCharts.R')

source('../common/dateUtilEL.R')
source('../common/getELENUM.R')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  
  # get data from JSON service
  dataObj = isolate(DGSData(session=session))
  #dataObj = isolate(DGSData(file="../test/testdata2.json"))

  Geburtsdatum <- as.Date(dataObj$get(requestedField=ELFIELD$person.geburtsdatum, type=ELTYPE$Ich))
  gender <- dataObj$get(requestedField=ELFIELD$person.geschlecht, type=ELTYPE$Ich)
  
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
  
#   # The following is done on change of input
#   output$overview <- renderPlot({    
#     
#     dist <- input$dist
#     n <- input$bu_annuity
#     
#     
#     par(mar = c(4,1,4,1), bg="white", mfcol=c(1,2))
#     
#     dialtext = paste(as.character(300 - n), ' EUR p.M.')
#     
#     dial.plot (label = "Sparen", value = 63-n/100, dial.radius = 0.7
#                , value.text = dialtext
#                , value.cex = 1.7
#                , label.cex = 1.7
#                , yellowFrom = 0, yellowTo = 50, yellow.slice.color = "red"
#                , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
#                , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
#     )
#     
#     title(main="Sparanteil in\n2016", cex.main = 2)
#     
#     
#     dial.plot (label = "Sparen", value = 33, dial.radius = 0.7
#                , value.text = "500 EUR p.M."
#                , value.cex = 1.7
#                , label.cex = 1.7
#                , yellowFrom = 0, yellowTo = 50, yellow.slice.color = "red"
#                , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
#                , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
#     )
#     
#     title(main="Sparen bei Renteneintritt\n2048", cex.main = 2)
#     
#     
#   })
#   
#   output$detailsInThreeTable <- renderPrint({
#     txt = "Einkommensbedarf pro Monat\n"
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%sLebensunterhalt                                        %8.2f EUR\n",txt, 1500)
#     txt = sprintf("%sVersicherungen                                         %8.2f EUR\n",txt, 200)
#     txt = sprintf("%sAltersvorsorge                                         %8.2f EUR\n",txt, 50)
#     txt = sprintf("%sAusbildung der Kinder                                  %8.2f EUR\n",txt, 500)
#     txt = sprintf("%sWohnen                                                 %8.2f EUR\n",txt, 750)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%sSumme (Ausgaben)                                       %8.2f EUR\n",txt, 3000)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%sEinnahmen pro Monat\n",txt)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%sErwerbsminderungsrente                                 %8.2f EUR\n",txt, 910)
#     txt = sprintf("%sBerufsgenossenschaft                                   %8.2f EUR\n",txt, 0)
#     txt = sprintf("%sBeamtenpension                                         %8.2f EUR\n",txt, 0)
#     txt = sprintf("%sbetriebliche Altersvorsorge                            %8.2f EUR\n",txt, 0)
#     txt = sprintf("%sArbeitseinkommen                                       %8.2f EUR\n",txt, 0)
#     txt = sprintf("%sArbeitseinkommen Partner                               %8.2f EUR\n",txt, 0)
#     txt = sprintf("%ssonst. Einnahmen                                       %8.2f EUR\n",txt, 0)
#     txt = sprintf("%sprivate Berufsunfaehigkeitsversicherung                %8.2f EUR\n",txt, 0)
#     txt = sprintf("%sprivate Unfallversicherung (%10.2f EUR) entspricht %8.2f EUR\n",txt, 100000, 100000*0.005)
#     txt = sprintf("%sverwertbares Vermoegen     (%10.2f EUR) entspricht %8.2f EUR\n",txt, 12000, 12000*0.005)
#     txt = sprintf("%sErbe                       (%10.2f EUR) entspricht %8.2f EUR\n",txt, 0, 0*0.005)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%sSumme (Einnahmen)                                      %8.2f EUR\n",txt, 1470)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%s\n",txt)
#     txt = sprintf("%sBedarf: BU Rente                                       %8.2f EUR\n",txt, 1670)
#     cat(txt) 
#   })
  
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
    
    Geburtsdatum <- as.Date(dataObj$get(requestedField=ELFIELD$person.geburtsdatum, type=ELTYPE$Ich))
    
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