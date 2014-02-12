require(shiny)
require(rCharts)
library(rjson)

source("../common/INIT.R", encoding="UTF-8", chdir=T)

source("../balance/computations.R", encoding="UTF-8")
source("../balance/render.R", encoding="UTF-8")


shinyServer(function(input, output,session) {
  #dataObj = isolate(DGSData(session=session))
  #dataObj = isolate(DGSData(file="../test/testdata2.json"))
  dataObj = isolate(DGSData(session))
  
  CashItm = getCashItm(dataObj)
  categorynames=colnames(CashItm)
  categorynames =categorynames[grepl("^taxonomy.*", categorynames)]
  aggregateString = makeAggregateString(categorynames);      
    
  assets = prepChartData(CashItm, "static",aggregateString)
  credit = prepChartData(CashItm, "credit",aggregateString)
  income = prepChartData(CashItm, "income",aggregateString)
  expense = prepChartData(CashItm, "expense",aggregateString)
  
  sumassets = sumNoNA(assets['wert'])
  sumcredit = sumNoNA(credit['wert'])
  sumincome = sumNoNA(income['wert'])
  sumexpense = sumNoNA(expense['wert'])
  
  
  output$ovFlowHtml <-renderUI(htmlFlow(sumincome,sumexpense))
  output$ovStatHtml <-renderUI(htmlStatic(sumassets, sumcredit))
  
  
  # asset
  output$myChartAssets <- renderChart({    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drillasset.html")
    theChart$addParams(data=assets)    
    return(theChart$copy())
  })
     
  
  output$myAssetTable <- renderUI({
    if(is.character(assets)) {
      res = data.frame(character(0))
      colnames(res) <- c("")
      return(head(res,n=0))      
    }
    
    toShow = assets
    filterstr = input$myassetlevel   
    toShow =filterTableData(toShow,filterstr,categorynames)     
    colnames(toShow) <- c("Kategorie 1","Kategorie 2","Kategorie 3","Objekt","Wert in \u20AC")    
    return(renderCatTable(toShow))
  })
  
  
  # credit etc
  output$myChartCredit <- renderChart({
    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drillcredit.html")
    theChart$addParams(data=credit)
    return(theChart$copy())
  })
  
  output$myCreditTable <- renderUI({
    if(is.character(credit)) {
      res = data.frame(character(0))
      colnames(res) <- c("")
      return(head(res,n=0))
    } 
    
    toShow = credit
    filterstr = input$mycreditlevel
    toShow =filterTableData(toShow,filterstr,categorynames)
        
    colnames(toShow) <- c("Kategorie 1","Kategorie 2","Kategorie 3","Objekt","Wert in \u20AC")
    
    return(renderCatTable(toShow))    
  })
  
  
  # income
  output$myChartIncome <- renderChart({
    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drillincome.html")
    theChart$addParams(data=income)
    return(theChart$copy())
  })
  output$myIncomeTable <- renderUI({
    if(is.character(income)) {
      res = data.frame( character(0))
      colnames(res) <- c("")
      return(head(res,n=0))
    } 
    
    toShow = income
    filterstr = input$myincomelevel
    toShow =filterTableData(toShow,filterstr,categorynames)
    colnames(toShow) <- c("Kategorie 1","Kategorie 2","Kategorie 3","Objekt","Wert in \u20AC")
    
    return(renderCatTable(toShow))
  })
  
  
  # expense
  output$myChartExpense <- renderChart({
    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drillexpense.html")
    theChart$addParams(data=expense)
    return(theChart$copy())
  })
  
  output$myExpenseTable <- renderUI({
    if(is.character(expense)) {
      res = data.frame(character(0))
      colnames(res) <- c("")
      return(head(res,n=0))
    }
    
    toShow = expense
    filterstr = input$myexpenselevel
    toShow =filterTableData(toShow,filterstr,categorynames)
    colnames(toShow) <- c("Kategorie 1","Kategorie 2","Kategorie 3","Objekt","Wert in \u20AC")
    return(renderCatTable(toShow))
  })
  
  
})

