library(shiny)

source('balance.R')
source('CashFlowAndBalanceSheetStackBarPlot.R')
source('CashFlowAndBalanceSheetLinePlot1.R')
source('gauge.R')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  
  # get data from JSON service
  url ='http://thetava.com/shiny-data/vertrag?sid=v69Sp5Cs25vJfwLOGbuW5y52nz0WukTMxUPSko4qPvA'
  CashStr = getItemsStructure(url)
  
  # convert data to data.frame
  CashItm = getItemsDataFrameFromStructure(CashStr)
  
  # compute time series from data.frame
  CashTS = getCashFlowTS(CashItm)
  
  
  # The following is done on change of input
  output$overview <- renderPlot({    
    
    dist <- input$dist
    n <- input$bu_annuity
    
    
    par(mar = c(4,1,4,1), bg="white", mfcol=c(1,2))
    
    dialtext = paste(as.character(300 - n), ' EUR p.M.')
    
    dial.plot (label = "Sparen", value = 63-n/100, dial.radius = 0.7
               , value.text = dialtext
               , value.cex = 1.7
               , label.cex = 1.7
               , yellowFrom = 0, yellowTo = 50, yellow.slice.color = "red"
               , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
               , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
    )
    
    title(main="Sparanteil in\n2016", cex.main = 2)
    
    
    dial.plot (label = "Sparen", value = 33, dial.radius = 0.7
               , value.text = "500 EUR p.M."
               , value.cex = 1.7
               , label.cex = 1.7
               , yellowFrom = 0, yellowTo = 50, yellow.slice.color = "red"
               , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
               , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
    )
    
    title(main="Sparen bei Renteneintritt\n2048", cex.main = 2)
    
    
  })
  
  output$detailsInThreeTable <- renderPrint({
    txt = "Einkommensbedarf pro Monat\n"
    txt = sprintf("%s\n",txt)
    txt = sprintf("%sLebensunterhalt                                        %8.2f EUR\n",txt, 1500)
    txt = sprintf("%sVersicherungen                                         %8.2f EUR\n",txt, 200)
    txt = sprintf("%sAltersvorsorge                                         %8.2f EUR\n",txt, 50)
    txt = sprintf("%sAusbildung der Kinder                                  %8.2f EUR\n",txt, 500)
    txt = sprintf("%sWohnen                                                 %8.2f EUR\n",txt, 750)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%sSumme (Ausgaben)                                       %8.2f EUR\n",txt, 3000)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%sEinnahmen pro Monat\n",txt)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%sErwerbsminderungsrente                                 %8.2f EUR\n",txt, 910)
    txt = sprintf("%sBerufsgenossenschaft                                   %8.2f EUR\n",txt, 0)
    txt = sprintf("%sBeamtenpension                                         %8.2f EUR\n",txt, 0)
    txt = sprintf("%sbetriebliche Altersvorsorge                            %8.2f EUR\n",txt, 0)
    txt = sprintf("%sArbeitseinkommen                                       %8.2f EUR\n",txt, 0)
    txt = sprintf("%sArbeitseinkommen Partner                               %8.2f EUR\n",txt, 0)
    txt = sprintf("%ssonst. Einnahmen                                       %8.2f EUR\n",txt, 0)
    txt = sprintf("%sprivate Berufsunfaehigkeitsversicherung                %8.2f EUR\n",txt, 0)
    txt = sprintf("%sprivate Unfallversicherung (%10.2f EUR) entspricht %8.2f EUR\n",txt, 100000, 100000*0.005)
    txt = sprintf("%sverwertbares Vermoegen     (%10.2f EUR) entspricht %8.2f EUR\n",txt, 12000, 12000*0.005)
    txt = sprintf("%sErbe                       (%10.2f EUR) entspricht %8.2f EUR\n",txt, 0, 0*0.005)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%sSumme (Einnahmen)                                      %8.2f EUR\n",txt, 1470)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%s\n",txt)
    txt = sprintf("%sBedarf: BU Rente                                       %8.2f EUR\n",txt, 1670)
    cat(txt) 
  })
  
  output$myChart <- renderChart2({
    
    Name_frame = c(character())
    Einkommen_frame= c(character())
    Wert_frame= c(numeric())
    
    Names = c(character())
    Names[1] = 'gesund'
    Names[2] = 'berufsunfaehig'
    
    
    Arbeitsvertrag = c(3000, 3000, 0)
    Erwerbsminderungsrente = c(915, 915)
    
    
    
    for(i in 1:length(Names)){
      
      if (i==1){
        Name_frame[(i-1)*3+1] = Names[i]
        Einkommen_frame[(i-1)*3+1] = "Arbeitsvertrag"
        Wert_frame[(i-1)*3+1] = Arbeitsvertrag[i] * 12 * 34
        
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
        Wert_frame[(i-1)*3+3] = Erwerbsminderungsrente[i] * 31 * 12
      }
    }
    
    
    
    phase = data.frame(Name_frame, Einkommen_frame, Wert_frame)
    colnames(phase) = c('Name', 'Lebensabschnitt', 'Dauer')
    
    n1 <- nPlot(Dauer ~ Name, data = phase, group = 'Lebensabschnitt', type = 'multiBarChart')
    n1$yAxis(tickFormat = "#!function(d) {return (d/1000000).toFixed(2) + ' Mio';}!#")
    n1$chart(width = 450)
    return(n1)
    
    ##################################################################################################
    
  })
  
  output$detailsInThree <- renderPlot({
    
    
    dist <- input$dist
    n <- input$bu_annuity
    
    #hist(data(), main=paste('r', dist, '(', n, ')', sep=''))
    #dial.plot(label = 'Sparen')
    
    # <- FROM HERE ->
    
    layout(mat=matrix(c(1,2,3,4,5,3), ncol=2), widths = c(2,1), heights=c(2,2,3))
    
    par(mar = c(1,2.8,1.5,1), bg="white", lwd =0.8)
    
    
    IncomeBalanceStackedBarPlot(CashTS)
    #IncomeBalanceLinePlot(CashTS)
    
    plot.new()
    
    
    text(x=0, y = 0.9, cex=1.2, adj=c(0,0),labels="Wie hoch ist mein Einkommen?")
    text(x=0, y = 0.75, cex=1.2, adj=c(0,0),labels="2300,00 EUR bis 20.10.2013: 6 Wochen Lohnfortzahung (Arbeitgeber)")
    text(x=0, y = 0.65, cex=1.2, adj=c(0,0),labels="1800,00 EUR bis 14.04.2015: max. 78 Wochen Krankengeld (Krankenkasse)")
    text(x=0, y = 0.55, cex=1.2, adj=c(0,0),labels="1100,00 EUR ab 14.04.2015: ggf. Rente")
    text(x=0, y = 0.45, cex=1.2, adj=c(0,0),labels=" 900,00 EUR ab 14.04.2048: staatliche Rente")
    
    par(mar = c(1,1,1.5,1), bg="white")
    
    dial.plot (label = "Sparen", value = 63, dial.radius = 0.7
               , value.text = "300 EUR p.M."
               , value.cex = 1.7
               , label.cex = 1.7
               , yellowFrom = 0, yellowTo = 50, yellow.slice.color = "red"
               , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
               , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
    )
    
    title(main="2016", cex.main = 2)
    
    
    dial.plot (label = "Sparen", value = 33, dial.radius = 0.7
               , value.text = "500 EUR p.M."
               , value.cex = 1.7
               , label.cex = 1.7
               , yellowFrom = 0, yellowTo = 50, yellow.slice.color = "red"
               , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
               , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
    )
    
    title(main="2048", cex.main = 2)
    
    
  })
  
})