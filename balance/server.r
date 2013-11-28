require(shiny)
require(rCharts)
library(rjson)

source("../bu/balance.R")
source("../common/readDGSData.R")

makeAggregateString<- function(categorynames) {
  aggregateString = 'wert~'
  for(i in 1:length(categorynames)) {
    if(i>1) aggregateString = paste(aggregateString,'+',categorynames[i],sep='') 
    else    aggregateString = paste(aggregateString,categorynames[i],sep='')      
  }
  aggregateString = paste(aggregateString,'+titel',sep='')  
}

prepChartData <- function(data, pBewertung,aggregateString ){
  result ="NoData"
  mdata = subset(data, bewertung == pBewertung)
  if(nrow(mdata)>0) 
    #result = aggregate(wert~taxonomy1+taxonomy2+taxonomy3+titel, sum, data=mdata)
    result = aggregate(formula(aggregateString), sum, data=mdata)
  result  
}

filterTableData <- function(data,filterstr,categories) {
 
  if(nchar(filterstr)>0) {
    filters = fromJSON(filterstr)
    
    if(length(filters)>0 && length(categories)>0 ) {
      flength=min(length(filters),length(categories))
      
      #CashItm[CashItm['taxonomy1']=="Mein Besitz", ]
      for (i in 1:flength) {
        data = data[data[categories[i]]== filters[i],]
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
  categorynames=colnames(CashItm)
  categorynames =categorynames[grepl("^taxonomy.*", categorynames)]
  aggregateString = makeAggregateString(categorynames);
  
  assets = prepChartData(CashItm, "static",aggregateString)
  credit = prepChartData(CashItm, "credit",aggregateString)
  income = prepChartData(CashItm, "income",aggregateString)
  expense = prepChartData(CashItm, "expense",aggregateString)
  
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
      res = data.frame(character(0))
      colnames(res) <- c("")
      return(head(res,n=0))
    }
    
    toShow = assets
    filterstr = input$myassetlevel
    print(filterstr)
    toShow =filterTableData(toShow,filterstr,categorynames)
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
      res = data.frame(character(0))
      colnames(res) <- c("")
      return(head(res,n=0))
    } 
    
    toShow = credit
    filterstr = input$mycreditlevel
    toShow =filterTableData(toShow,filterstr,categorynames)
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
      res = data.frame( character(0))
      colnames(res) <- c("")
      return(head(res,n=0))
    } 
    
    toShow = income
    filterstr = input$myincomelevel
    toShow =filterTableData(toShow,filterstr,categorynames)
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
      res = data.frame(character(0))
      colnames(res) <- c("")
      return(head(res,n=0))
    }
    
    toShow = expense
    filterstr = input$myexpenselevel
    toShow =filterTableData(toShow,filterstr,categorynames)
    head(toShow,n=nrow(toShow))
  })

  
})

