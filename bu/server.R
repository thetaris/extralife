library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  
  data <- reactive({  
    dist <- switch(input$cause,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$bu_annuity)
  })
  
  # Store in a convenience variable
  cdata <- session$clientData
  myData <- rnorm(1)
  
  # Values from cdata returned as text
  output$clientdataText <- renderText({
    cnames <- names(cdata)
    
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep=" = ")
    })
    paste(allvalues, collapse = "\n")
    #paste(paste(allvalues, collapse = "\n"), as.character(myData),  collapse = "\n")    
  })
  
  output$distPlot <- renderPlot({
    
    # get data from JSON service
    url ='http://thetava.com/shiny-data/vertrag?sid=5WJC8LCCHDBPghdOAgCgqmnV-3GgaxvdsIP_rL4rbjM'
    CashStr = getItemsStructure(url)
    
    # convert data to data.frame
    CashItm = getItemsDataFrameFromStructure(newStr)
    
    # compute time series from data.frame
    CashTS = getCashFlowTS(newItm)
    
    
    dist <- input$dist
    n <- input$bu_annuity
    
    #hist(data(), main=paste('r', dist, '(', n, ')', sep=''))
    #dial.plot(label = 'Sparen')
    
    # <- FROM HERE ->
    
    layout(mat=matrix(c(1,2,3,4,5,3), ncol=2), widths = c(2,1), heights=c(2,2,3))
    
    par(mar = c(1,2.8,1.5,1), bg="white", lwd =0.8)
    
    
    #IncomeBalanceStackedBarPlot(CashTS)
    IncomeBalanceLinePlot(CashTS)
    
    plot.new()
    
    
    text(x=0, y = 0.9, cex=1.2, adj=c(0,0),labels="Wie hoch ist mein Einkommen?")
    text(x=0, y = 0.75, cex=1.2, adj=c(0,0),labels="2300,00 EUR bis 20.10.2013: 6 Wochen Lohnfortzahung (Arbeitgeber)")
    text(x=0, y = 0.65, cex=1.2, adj=c(0,0),labels="1800,00 EUR bis 14.04.2015: max. 78 Wochen Krankengeld (Krankenkasse)")
    text(x=0, y = 0.55, cex=1.2, adj=c(0,0),labels="1100,00 EUR ab 14.04.2015: ggf. Rente")
    text(x=0, y = 0.45, cex=1.2, adj=c(0,0),labels=" 900,00 EUR ab 14.04.2048: staatliche Rente")
    
    par(mar = c(1,1,1.5,1), bg="white")
    
    dial.plot (label = "Sparen", value = 63, dial.radius = 1
               , label.cex = 1.7
               , yellowFrom = 0, yellowTo = 40, yellow.slice.color = "red"
               , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
               , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
    )
    
    title(main="2016")
    
    
    dial.plot (label = "Sparen", value = 33, dial.radius = 1
               , label.cex = 1.7
               , yellowFrom = 0, yellowTo = 40, yellow.slice.color = "red"
               , redFrom = 60, redTo = 100, red.slice.color = "olivedrab"
               , needle.color = "red", needle.center.color = "black", needle.center.cex = 1
    )
    
    title(main="2048")
    

  })
  
})