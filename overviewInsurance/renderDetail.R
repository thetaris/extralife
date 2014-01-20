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
