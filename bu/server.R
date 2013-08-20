library(shiny)

source('balance.R')
source('CashFlowAndBalanceSheetStackBarPlot.R')
source('CashFlowAndBalanceSheetLinePlot1.R')
source('gauge.R')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  
  
  # get data from JSON service
  url ='http://thetava.com/shiny-data/vertrag?sid=5WJC8LCCHDBPghdOAgCgqmnV-3GgaxvdsIP_rL4rbjM'
  CashStr = getItemsStructure(url)
  
  # convert data to data.frame
  CashItm = getItemsDataFrameFromStructure(CashStr)
  
  # compute time series from data.frame
  CashTS = getCashFlowTS(CashItm)
  
  
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
    
    title(main="2016")
    
    
    dial.plot (label = "Sparen", value = 33, dial.radius = 0.7
               , value.text = "500 EUR p.M."
               , value.cex = 1.7
               , label.cex = 1.7
               , yellowFrom = 0, yellowTo = 50, yellow.slice.color = "red"
               , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
               , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
    )
    
    title(main="2048")
    

  })
  
})