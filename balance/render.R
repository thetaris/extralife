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