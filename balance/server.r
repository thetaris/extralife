require(shiny)
require(rCharts)
library(rjson)

source("../bu/balance.R")
source("../common/readDGSData.R")
source('../common/getELFIELD.R')
source("../common/utilEL.R", encoding="UTF-8")

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
  
  htmlFlow   = paste('<h3>monatliche Ausgaben und Einnahmen</h3>
                     <table rules="rows" style="white-space: nowrap" width="500px">                     
                     <tr onclick="jumpIncome()" ><td><a>Summe der Einnahmen</a></td><td>'
                     ,renderEuro(sumincome)
                     ,'</td></tr><tr onclick="jumpExpense()"><td><a>Summe der Ausgaben</a></td><td>'
                     ,renderEuro(sumexpense)
                     ,'</td></tr><tr><td>&Uuml;berschuss</td><td>'
                     ,renderEuro(sumflow)
                     , '</td></tr></table>')

  htmlStatic = paste('<h3>Verm&ouml;gens&uuml;bersicht</h3>
                     <table rules="rows" style="white-space: nowrap" width="500px">
                     <tr onclick="jumpAsset()" ><td><a>Summe der Verm&ouml;genswerte</a></td><td>'
                     ,renderEuro(sumassets)
                     ,'</td></tr><tr onclick="jumpCredit()"><td><a>Summe der Kredite</a></td><td>'
                     ,renderEuro(sumcredit)
                     ,'</td></tr><tr><td>Netto Verm&ouml;gen</td><td>'
                     ,renderEuro(sumstatic)
                     , '</td></tr></table>')
  
  
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
  
  
  renderCatTable<-function(toShow){    
      tmp = list(tags$th(tags$h4("Kategorie")), tags$th(tags$h4("Objekt")), tags$th(tags$h4("Wert")))    
      res = tags$tr(tmp)
      
      for (iter in 1:nrow(toShow))
      {
        tmp = list(tags$th(toShow[iter,"Kategorie 3"], align="left")
                   ,tags$th(toShow[iter,"Objekt"], align="left")
                   ,tags$th(renderEuro( toShow[iter,"Wert in \u20AC"]), align="right")
                   )    
        
        res = list(res, tags$tr(tmp))
      }              
      res = tags$table(res, rules="rows", cellpadding="5%", align="left", width="600px", style="white-space: nowrap")
    return(res)
  }  
  
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

