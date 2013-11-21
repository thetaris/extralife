require(shiny)
require(rCharts)
library(rjson)

source("../bu/balance.R")
source("../common/readDGSData.R")

shinyServer(function(input, output, session) {
  
  dataObj = DGSData(session=session)
  #dataObj = DGSData(file="../test/testdata2.json")
  
  CashItm = getCashItm(dataObj)
  
  allData=aggregate(wert~taxonomy1+taxonomy2+taxonomy3, sum, data=CashItm)
  
  mdata = subset(CashItm, bewertung == "static")
  assets=aggregate(wert~taxonomy1+taxonomy2+taxonomy3+titel, sum, data=mdata)
  
  mdata = subset(CashItm, bewertung == "credit")
  credit=aggregate(wert~taxonomy1+taxonomy2+taxonomy3+titel, sum, data=mdata)
  
  mdata=subset(CashItm, bewertung == "income")
  income=aggregate(wert~taxonomy1+taxonomy2+taxonomy3+titel, sum, data=mdata)
  
  mdata=subset(CashItm, bewertung == "expense")
  expense=aggregate(wert~taxonomy1+taxonomy2+taxonomy3+titel, sum, data=mdata)
  
  
  output$myChartAssets <- renderChart({
    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drill1.html")
    theChart$addParams(data=assets)
    return(theChart$copy())
  })
  
  output$myChartCredit <- renderChart({
    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drill2.html")
    theChart$addParams(data=credit)
    return(theChart$copy())
  })
  
  output$myChartIncome <- renderChart({
    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drill3.html")
    theChart$addParams(data=income)
    return(theChart$copy())
  })
  
  output$myChartExpense <- renderChart({
    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drill4.html")
    theChart$addParams(data=expense)
    return(theChart$copy())
  })
})


