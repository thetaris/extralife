renderOverview <- function(versicherungen, besitz, familie, input)
renderUI({
  renderSchaden <- function(logSum){
    symbol = tags$img(src="http://thetava.com/Shiny_icons/euro.png", width="25", border="0")
    space = tags$a(".")
    
    res = list();
    if (any(logSum>0)){
      for (iter in 1:logSum){
        res = list(symbol, res)        
        if ( ((iter %% 3) == 0) & (iter<logSum) ){
          res = list(space, res)
        }
      }      
    }
    return(tags$div(res))
  }
  
  renderAbdeckung <- function(type){
    # type is 0 to 5 (best)
    return(tags$img(src=sprintf("http://thetava.com/Shiny_icons/Signal_%i.png",type), width="25"))
  }
  
  renderStatus <- function(type=0){
    if (type==0) {
      return(tags$img(src="http://thetava.com/Shiny_icons/Check_Icon_32.png"))
    } else {
      return(tags$img(src="http://thetava.com/Shiny_icons/Warning_Icon_32.png"))
    }
  }
  versicherungen <- getEmpfehlungen(versicherungen, besitz, familie, input)
  
  tmp = list(tags$th(tags$h4("Risiko")), tags$th(tags$h4("möglicher Schaden")), tags$th(tags$h4("Abdeckung")), tags$th(tags$h4("Status")))    
  res = tags$tr(tmp)
  
  #for (iterRisk in names(versicherungen))
  for (iterRisk in order(sapply(versicherungen, function(x) -x$schaden, USE.NAMES = T)))
  {
    tmp = list(tags$th(div(id= versicherungen[[iterRisk]]$link
                           ,tags$a( versicherungen[[iterRisk]]$titel)), align="left")
               ,tags$th(renderSchaden(versicherungen[[iterRisk]]$schaden), align="right")
               ,tags$th(renderAbdeckung( versicherungen[[iterRisk]]$abdeckung))
               ,tags$th(renderStatus( versicherungen[[iterRisk]]$status)))    
    
    res = list(res, tags$tr(tmp))
  }              
  res = tags$table(res, rules="rows", cellpadding="5%", align="center")
  
  scripts = list()
  # iterate over indices of names which is consistent with Tab numbering in JavaScript
  for (iterRisk in 1:length(names(versicherungen)))
  {
    
    scripts = list(scripts, HTML(sprintf("$('#%s').click(function() {
                   tabs = $('.tabbable .nav.nav-tabs li')
      					 	 tabs.each(function() {
      							$(this).removeClass('active')
      					 	 })
      						 $(tabs[%i]).addClass('active')
      						
      						 tabsContents = $('.tabbable .tab-content .tab-pane')
      					 	 tabsContents.each(function() {
      							$(this).removeClass('active')
      					 	 })
      						 $(tabsContents[%i]).addClass('active')
      						
      						$('#%s').trigger('change').trigger('shown');
      					 })
        			", versicherungen[[iterRisk]]$link,iterRisk, iterRisk,versicherungen[[iterRisk]]$link)))
  }
  res = list(res,tags$script(scripts))
  
})

renderDetail <- function(versicherungen, besitz, familie, input, type){
  renderUI({
    
    renderContracts <- function(vertraegeTabelle){
      tmp <- list(tags$th(tags$h4("Vertrag")), tags$th(tags$h4("Versicherungsnehmer")), tags$th(tags$h4("monatl. Kosten")))    
      res <- tags$tr(tmp)
      
      if (nrow(vertraegeTabelle)>0){
        for (iterVertrag in 1:nrow(vertraegeTabelle))        
        {
          tmp <- list(tags$th(vertraegeTabelle[iterVertrag, "Vertragsname"], align="left")
                      ,tags$th(vertraegeTabelle[iterVertrag, "Versicherungsnehmer"], align="right")
                      ,tags$th(renderEuro(vertraegeTabelle[iterVertrag, "Kosten"]), align="right")    
                      # ,tags$th((vertraegeTabelle[iterVertrag, "Kosten"]), align="right")    
          )            
          res <- list(res, tags$tr(tmp))
        }     
      }else{
        res <- list(res, tags$tr(tags$th("keine")))
      }
      res <- tags$table(res, rules="rows", cellpadding="10%", align="center")      
      return(res)
    }
    
    renderPage <- function(versicherung){
      absicherung=versicherung$absicherung
      empfehlung=versicherung$empfehlung
      details=versicherung$detailsURL                          
      linkBdV=versicherung$linkBdV
      linkCosmos = versicherung$linkCosmos
      
      page <- list()
      if (absicherung=="ok")
      {
       page<-list(page,tags$h3(sprintf("Absicherung: %s", absicherung)))
      } else {
        page<-list(page,tags$div( tags$h3(sprintf("Absicherung: %s", absicherung)), style="color:#FF0000"))        
      }
      page<-list(page,tags$h3("Empfehlung")
                 ,tags$p(empfehlung)
      )
      if (!is.null(details)){
        page<-list(page,tags$a("Details", href=details))
      }
      if ((!is.null(linkBdV)) | (!is.null(linkCosmos))){
        page<-list(page,tags$h3("Weiterführende Informationen"))
        if (!is.null(linkBdV)){
          tmp<-list()
          for (iterContract in c(1:length(linkBdV$URL))){
            tmp<-list(tmp, tags$a(linkBdV$text[iterContract], href=linkBdV$URL[iterContract], target="_blank"), tags$br())
          }
          page<-list(page, tags$div("Bund der Versicherten: ",tmp, align="right", style="font-weight:bold"))
        }
        if (!is.null(linkCosmos)){
          page<-list(page,div("Stiftung Warentest: ", linkCosmos$HTML, align="right", style="font-weight:bold"))
          
        }
      }
      return(page)
    }
    versicherungen<-getEmpfehlungen(versicherungen, besitz, familie, input)
    versicherung <- versicherungen[[type]]
    
    # create table in variable res    
    
    res <- renderContracts(versicherung$vertraegeTabelle)
    
    res<-list(res
              ,renderPage(versicherung)
    )
    return(res)
  })
}
