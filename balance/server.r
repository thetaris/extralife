require(shiny)
require(rCharts)
library(rjson)

source("../bu/balance.R")
source("../common/readDGSData.R")

prepChartData <- function(data, pBewertung ){
  result ="NoData"
  
  mdata = subset(data, bewertung == pBewertung)
  if(nrow(mdata)>0) 
    result=aggregate(wert~taxonomy1+taxonomy2+taxonomy3+titel, sum, data=mdata)
  
  result  
}

filterTableData <- function(data,filterstr) {
 
  if(nchar(filterstr)>0) {
    filters = fromJSON(filterstr)
    
    if(length(filters)>0) {
      
      for (i in 1:length(filters)) {
        print(filters[i])
        if(i==1)          data = subset(data, taxonomy1 == filters[1])    
        if(i==2)          data = subset(data, taxonomy2 == filters[2])   
        if(i==3)          data = subset(data, taxonomy3 == filters[3])
      }
    }
  }
 
  return(data)
}

shinyServer(function(input, output,session) {
  #dataObj = isolate(DGSData(session=session))
  #dataObj = isolate(DGSData(file="../test/testdata2.json"))
  dataObj = isolate(DGSData(session))

  #dataObj = isolate(DGSData(sid="NDvJD2-LPQ42nt4JcHM4TMG-OWXY18ZraNEb3rV7Qd4"))
  
  CashItm = getCashItm(dataObj)
  print(CashItm)
  allData=aggregate(wert~taxonomy1+taxonomy2+taxonomy3, sum, data=CashItm)
  print(allData)
  assets = prepChartData(CashItm, "static")
  credit = prepChartData(CashItm, "credit")
  print(credit)
  income = prepChartData(CashItm, "income")
  expense = prepChartData(CashItm, "expense")
  
  # asset
  output$myChartAssets <- renderChart({
    
    # lokal subdir thetadrill
    theChart <- rCharts$new()
    theChart$setLib('thetadrill')
    theChart$setTemplate(script = "thetadrill/layouts/drillasset.html")
    theChart$addParams(data=assets)
    return(theChart$copy())
  })
  
  output$myAssetTable <- renderTable({
    if(is.character(assets)) {
      res = data.frame(numeric(0), character(0))
      colnames(res) <- c("", "")
      return(head(res,n=0))
    }
    
    toShow = assets
    filterstr = input$myassetlevel
    print(filterstr)
    toShow =filterTableData(toShow,filterstr)
    head(toShow,n=nrow(toShow))
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
  output$myCreditTable <- renderTable({
    if(is.character(credit)) {
      res = data.frame(numeric(0), character(0))
      colnames(res) <- c("", "")
      return(head(res,n=0))
    } 
    
    toShow = credit
    filterstr = input$mycreditlevel
    toShow =filterTableData(toShow,filterstr)
    head(toShow,n=nrow(toShow))
    
    
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
  output$myIncomeTable <- renderTable({
    if(is.character(income)) {
      res = data.frame(numeric(0), character(0))
      colnames(res) <- c("", "")
      return(head(res,n=0))
    } 
    
    toShow = income
    filterstr = input$myincomelevel
    toShow =filterTableData(toShow,filterstr)
    head(toShow,n=nrow(toShow))
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
  output$myExpenseTable <- renderTable({
    if(is.character(expense)) {
      res = data.frame(numeric(0), character(0))
      colnames(res) <- c("", "")
      return(head(res,n=0))
    }
    
    toShow = expense
    filterstr = input$myexpenselevel
    toShow =filterTableData(toShow,filterstr)
    head(toShow,n=nrow(toShow))
  })

  
})

