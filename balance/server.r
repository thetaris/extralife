require(shiny)
require(rCharts)
library(rjson)

source("../bu/balance.R")
source("../common/readDGSData.R")
source('../common/getELFIELD.R')

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
 print(filterstr)
 
  if(nchar(filterstr)>0) {
    filters = fromJSON(filterstr)
    print(filters)
    
    if(length(filters)>0 && length(categories)>0 ) {
      flength=min(length(filters),length(categories))
      print(flength)  
      #CashItm[CashItm['taxonomy1']=="Mein Besitz", ]
      for (i in 1:flength) {
        print('vor')
        data = data[data[categories[i]]== filters[i],]
        print(data)
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
  
  sumassets=0;
  if(!is.character(assets)) {
    sumassets = sum(assets['wert'])
  }
  sumcredit=0;
  if(!is.character(credit)) {
    sumcredit = sum(credit['wert'])
  }
  sumincome=0;
  if(!is.character(income)) {
    sumincome = sum(income['wert'])
  }
  sumexpense=0;
  if(!is.character(expense)) {
    sumexpense = sum(expense['wert'])
  }  
  
  sumstatic = sumassets-sumcredit
  sumflow = sumincome-sumexpense
  
  # overview 

  htmlStatic = paste('<table><tr onclick="jumpAsset()" ><td>Summe der Verm&ouml;genswerte</td><td>',sumassets,'</td></tr><tr onclick="jumpCredit()"><td>Summe der Kredite</td><td>',sumcredit,'</td></tr><tr><td>Netto Verm&ouml;gen</td><td>', sumstatic, '</td></tr></table>')
  htmlFlow   = paste('<table><tr onclick="jumpIncome()" ><td>Summe der Einnahmen</td><td>',sumincome,'</td></tr onclick="jumpExpense()"><tr><td>Summe der Ausgaben</td><td>',sumexpense,'</td></tr><tr><td>&Uuml;berschuss</td><td>', sumflow, '</td></tr></table>')
  
  output$ovFlowHtml <-renderText({ htmlFlow})
  output$ovStatHtml <-renderText({ htmlStatic})
  
  
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
    toShow =filterTableData(toShow,filterstr,categorynames)   
    colnames(toShow) <- c("Kategorie 1","Kategorie 2","Kategorie 3","Objekt","Wert in \u20AC")
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
    print('rendertable')
    print(credit)
    if(is.character(credit)) {
      res = data.frame(character(0))
      colnames(res) <- c("")
      print(res)
      return(head(res,n=0))
    } 
    
    toShow = credit
    print(toShow)
    filterstr = input$mycreditlevel
    print(filterstr)
    toShow =filterTableData(toShow,filterstr,categorynames)
    print(toShow)
    colnames(toShow) <- c("Kategorie 1","Kategorie 2","Kategorie 3","Objekt","Wert in \u20AC")
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
    colnames(toShow) <- c("Kategorie 1","Kategorie 2","Kategorie 3","Objekt","Wert in \u20AC")
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
    colnames(toShow) <- c("Kategorie 1","Kategorie 2","Kategorie 3","Objekt","Wert in \u20AC")
    head(toShow,n=nrow(toShow))
  })

  
})

