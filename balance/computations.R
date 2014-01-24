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
#   the following might be required
#   if(is.character(result)){      
#     result=data.frame(wert=0)    
#   }
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

htmlFlow = function(sumincome, sumexpense){
  (tags$html(tags$h3("monatliche Ausgaben und Einnahmen"),
                      tags$table(rules="rows", style="white-space: nowrap", width="500px"
                                 ,tags$tr(onclick="jumpIncome()"
                                          ,tags$td(tags$a("Summe der Einnahmen"))                                     
                                          ,tags$td(renderEuro(sumincome))
                                 )
                                 ,tags$tr(onclick="jumpExpense()"
                                          ,tags$td(tags$a("Summe der Ausgaben"))
                                          ,tags$td(renderEuro(sumexpense))
                                 )
                                 ,tags$tr(   tags$td("Überschuss")
                                             ,tags$td(renderEuro(sumincome-sumexpense))
                                 )
                      )
))
}

htmlStatic = function(sumassets, sumcredit){
  (tags$html(tags$h3("Vermögensübersicht"),
                        tags$table(rules="rows", style="white-space: nowrap", width="500px"
                                   ,tags$tr(onclick="jumpAsset()"
                                            ,tags$td(tags$a("Summe der Vermögenswerte"))                                     
                                            ,tags$td(renderEuro(sumassets))
                                   )
                                   ,tags$tr(onclick="jumpCredit()"
                                            ,tags$td(tags$a("Summe der Kredite"))
                                            ,tags$td(renderEuro(sumcredit))
                                   )
                                   ,tags$tr(   tags$td("Netto Vermögen")
                                               ,tags$td(renderEuro(sumassets-sumcredit))
                                   )
                        )
))
}