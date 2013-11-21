library(rjson)

getItemsTaxonomyAndAttributes <- function(data, nameAttrib, tax = character() )
{
  result = character()
  
  tmp_fields <- attributes(data)  
  
  
  if (is.list(tmp_fields))
  {
    tmp_fields = tmp_fields[[1]]
    
    if (sum(tmp_fields=='children')>0){      
      result = c(result , getItemsTaxonomyAndAttributes(data$children, nameAttrib, c(tax, data$name)))
    }  
    
    if (sum(tmp_fields=='items')>0)
    {
      
      if (length(data$items)>0)
      {                  
        elements = data$items 
        for (i in 1:length(elements)) 
        {
          
          if (nameAttrib == 'taxonomy1')
            tmp_res = tax[1]
          else if (nameAttrib == 'taxonomy2')
            tmp_res = tax[3]
          else if (nameAttrib == 'taxonomy3')
            tmp_res = tax[5]
          else if (nameAttrib == 'wikipedia')
            tmp_res = data$wikipedia
          else if (nameAttrib == 'bewertung')
            tmp_res = data$bewertung
          else
          {                                                                                      
            tmp_res = c(getItemsAttributes(elements[[i]], nameAttrib))
          }
          
          if (length(tmp_res)==0)
            tmp_res='NULL'
          
          result = c(result, tmp_res)
        }
        
      }
    }    
  } 
  else
  {
    if (length(data)>0)
    { 
      # at root level
      for (i in 1:length(data)) {
        result = c(result, getItemsTaxonomyAndAttributes(data[[i]], nameAttrib, c(tax, data[[i]]$name)))
      }
    }
  }
  
  result
}

getItemsAttributes <- function(data, nameAttrib)
{
  
  if (sum(nameAttrib=='aktueller_wert')>0)
  {
    tmp_res = as.character(data$aktueller_wert$amount)
  } 
  else if (sum(nameAttrib=='betrag')>0)
  {
    tmp_res = as.character(data$betrag$amount)
    
  } 
  else 
  {
    tmp_res = as.character(data[nameAttrib])
  }
  
  if (length(tmp_res)==0)
  {
    tmp_res = 'NULL'
  }
  result = tmp_res                          
}


# getCashflows <- function(data){
#   titel = c('Mein Arbeitsvertrag', 'Standard GKV')
#   taxonomy1 = c('Vertrag', 'Vertrag')
#   taxonomy2 = c('Einkommen', 'Versicherungen')
#   taxonomy3 = c("Arbeitsvertrag", "Gesetzliche Krankenkasse")
#   wert = c(5000, 0)
#   zahlungsbeginn = c(as.Date("2013-07-22 00:00:00"), as.Date("2013-08-22 00:00:00"))
#   zahlungsbetrag = c(2000, 640)
#   zahlungswiederholung = c("monatlich", "monatlich")
#   zahlungsende = c(as.Date("2014-07-22 00:00:00"), as.Date("2015-07-22"))
#   befristung = c(0, 0)
#   bewertung = c('income', 'static')
#   result = data.frame(titel, taxonomy1, taxonomy2, wert, zahlungsbeginn, zahlungsbetrag, zahlungswiederholung, zahlungsende, befristung, bewertung)
# }


getItemsDataFrameFromStructure <- function(data){
  
  titel =                          getItemsTaxonomyAndAttributes(data, nameAttrib="name")  
  wert =               as.numeric( getItemsTaxonomyAndAttributes(data, nameAttrib="aktueller_wert"))
  
  zahlungsbeginn =               ( getItemsTaxonomyAndAttributes(data, nameAttrib="zahlungsbeginn"))
  zahlungsbeginn[zahlungsbeginn=='NULL'] = NA
  zahlungsbeginn =                 as.Date(zahlungsbeginn)
  
  zahlungsende =          ( getItemsTaxonomyAndAttributes(data, nameAttrib="ende"))
  zahlungsende[zahlungsende=='NULL'] = NA
  zahlungsende = as.Date(zahlungsende)
  
  zahlungsbetrag =     as.numeric( getItemsTaxonomyAndAttributes(data, nameAttrib="betrag"))
  zahlungswiederholung =           getItemsTaxonomyAndAttributes(data, nameAttrib="wiederholung")
  
  befristung =         as.numeric( getItemsTaxonomyAndAttributes(data, nameAttrib="befristung"))
  befristung[is.na(befristung)] = 0
  
  bewertung =                      getItemsTaxonomyAndAttributes(data, nameAttrib="bewertung")
  
  taxonomy1 =                      getItemsTaxonomyAndAttributes(data, nameAttrib="taxonomy1")
  taxonomy2 =                      getItemsTaxonomyAndAttributes(data, nameAttrib="taxonomy2")
  taxonomy3 =                      getItemsTaxonomyAndAttributes(data, nameAttrib="taxonomy3")
  
  result = data.frame(titel, taxonomy1, taxonomy2, taxonomy3, wert, zahlungsbeginn, zahlungsbetrag, zahlungswiederholung, zahlungsende, befristung, bewertung)
}

reportLog <- function(message, category = "error"){
  resStr = cat(as.character(Sys.time()), ' ', sep="")
  resStr = cat(resStr, category, sep=":")
  resStr = cat(resStr, " ", sep="")
  resStr = cat(resStr, message, sep='')
  resStr = cat(resStr, '\n', sep='')
  print(resStr)
}

getCashFlowTS <- function(dataFrame, NumMonths = 36){
  
  today = Sys.Date()  
  
  sparen              = seq(0,0,length.out=NumMonths)
  einkommen           = seq(0,0,length.out=NumMonths)
  konsum              = seq(0,0,length.out=NumMonths)
  balance_assets      = seq(0,0,length.out=NumMonths)
  balance_liabilities = seq(0,0,length.out=NumMonths)
  
  for (i in 1:length(dataFrame[,1])) 
  {
    
    element = dataFrame[i,]
    
    result <- tryCatch({
      
      firstIndex = max(as.numeric(difftime(element$zahlungsbeginn, today, units = "days"))*(12/365),0)
      
      if (is.na(firstIndex))
        firstIndex = 1
      
      if (is.na(element$wert))
        element$wert = 0
      
      
      if (is.na(element$zahlungsbetrag))
        element$zahlungsbetrag = 0

      if (is.na(element$befristung))
        element$befristung = 0      
      
      if (element$befristung == 0){
        firstIndex= 1
        lastIndex = NumMonths
      }else{
        lastIndex = min(max(as.numeric(difftime(element$zahlungsende, today, units = "days"))*(12/365),0),NumMonths)
      }
      if (element$bewertung=="saving"){
        # ignore since sparen = einkommen - expense
        #sparen[firstIndex:lastIndex] = sparen[firstIndex:lastIndex] + element$zahlungsbetrag      
      }else if (element$bewertung=="static"){
        konsum[firstIndex:lastIndex] = konsum[firstIndex:lastIndex] + element$zahlungsbetrag        
      }else if (element$bewertung=="expense"){
        konsum[firstIndex:lastIndex] = konsum[firstIndex:lastIndex] + element$zahlungsbetrag        
      }else if (element$bewertung=="credit"){      
        konsum[firstIndex:lastIndex] = konsum[firstIndex:lastIndex] + element$zahlungsbetrag        
        
      }else if (element$bewertung=="income"){
        einkommen[firstIndex:lastIndex] = einkommen[firstIndex:lastIndex] + element$zahlungsbetrag  
      }else{
        message = cat('Unknown Bewertung', as.character(element$bewertung), sep = ' ')
        message = cat(as.character(element$titel), message, ':')
        message = cat(message, ' ', sep=' ')
        reportLog(message)
      }
      if (as.numeric(element$wert>0))
      {
        if (element$bewertung=="credit")
        {   
          balance_liabilities[1] = balance_liabilities[1] + as.numeric(element$wert)
        }else{
          balance_assets[1] = balance_assets[1] + element$wert
        }
      }
      
    }, error = function(err){
      message = cat(as.character(element$titel), as.character(err), sep=" ")
      reportLog(as.character(message))
      
    })
    
  }
  
  sparen = einkommen - konsum
  
  # adjust balance by returns
  for (i in 2:NumMonths){
    if (sparen[i-1]>0){
      balance_assets[i]      = balance_assets[i-1]      + sparen[i-1]
      balance_liabilities[i] = balance_liabilities[i-1]         
    } else
    {
      balance_assets[i]      = balance_assets[i-1]      
      balance_liabilities[i] = balance_liabilities[i-1] - sparen[i-1]              
    }
  }
  
  
  balance_net = balance_assets - balance_liabilities
  
  month = as.numeric(as.character.Date(today,"%m"))
  year = as.numeric(as.character.Date(today,"%Y"))
  
  sparen              = ts(sparen,              frequency = 12, start = c(year, month))
  einkommen           = ts(einkommen,           frequency = 12, start = c(year, month))
  konsum              = ts(konsum,              frequency = 12, start = c(year, month))
  balance_assets      = ts(balance_assets,      frequency = 12, start = c(year, month))
  balance_net         = ts(balance_net,         frequency = 12, start = c(year, month))
  balance_liabilities = ts(balance_liabilities, frequency = 12, start = c(year, month))
  
  result = list("einkommen"=einkommen, "konsum"=konsum, "sparen"=sparen, "balance_assets"=balance_assets, "balance_liabilities"=balance_liabilities, "balance_net" = balance_net)
  
}

getItemsStructure <- function(url)
{
  fromJSON(file=url)
}


# get data from JSON service 
# Note: Set "sid" to a valid session ID for testing


#CashStr = getItemsStructure('http://cloud.thetaris.com/shiny-data/vertrag?sid=EsOILW7UJcLDc_B_s2-OQeVgiACf3H0SqFswZZqPbm4')
CashStr = getItemsStructure('http://thetava.com/shiny-data/vertrag?sid=iJ7tEMA1XeQCiAZewFrFgS9JUkLsvdRTDrnK5kynaxA')

# convert data to data.frame
CashItm = getItemsDataFrameFromStructure(CashStr)

# compute time series from data.frame
CashTS = getCashFlowTS(CashItm, 36)

#IncomeBalanceStackedBarPlot(CashTS)



