getVersicherungen <- function(dataObj){
  getData<- function(type)  {
    tmpVertrag = list()
    tmp.titel  = dataObj$get(type=type, requestedField=ELFIELD$title)  
    if (is.null(tmp.titel))
    {
      tmpVertrag$vertraegeTabelle = data.frame(matrix(NA, nrow = 0, ncol = 3))
    }else{
      tmp.kosten = dataObj$get(type=type, ELFIELD$i.kosten.monatlich)  
      tmp.ich    = dataObj$get(type=ELTYPE$Ich, requestedField=ELFIELD$title)      
      tmpVertrag$vertraegeTabelle = data.frame(  Vertragsname        = tmp.titel
                                                 , Versicherungsnehmer = rep(tmp.ich,length(tmp.titel))
                                                 , Kosten = tmp.kosten
                                                 , stringsAsFactors=FALSE
      )
      
    }
      
    return(tmpVertrag)
  }
  
  
  
  versicherungen <- list()
  
  # private Haftpflicht
  haftpflicht <- list()
 
  versicherungen$haftpflicht         = getData(type=ELTYPE$Privathaftpflichtversicherung)  
  versicherungen$Krankheit           = getData(type=ELTYPE$Krankenversicherung._)
  versicherungen$Invaliditaet        = getData(type=ELTYPE$Invaliditaetsversicherung._)
  versicherungen$Tod                 = getData(type=ELTYPE$Lebensversicherung._)
  versicherungen$SchadenAmAuto       = getData(type=ELTYPE$KFZ.Kaskoversicherung)
  versicherungen$KFZHaftpflicht      = getData(type=ELTYPE$KFZ.Haftpflichtversicherung)
  versicherungen$SchadenAmEigentum   = getData(type=ELTYPE$Hausratversicherung)
  versicherungen$Rechtsstreit        = getData(type=c( ELTYPE$Rechtsschutzversicherung, ELTYPE$Berufsverband ))
  return(versicherungen)
}

renderEuro <- function(betrag){
  if (is.character(betrag)){
    betrag <- as.numeric(betrag)
  }
  sprintf("%1.2f EUR", betrag)
}

renderDetail <- function(versicherung, type){
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
  
    renderPage <- function(absicherung, empfehlung, details=NULL){
      page <- list()
      page<-list(page,tags$h3(sprintf("Absicherung: %s", absicherung)))
      page<-list(page,tags$h3("Empfehlung:")
                   ,tags$p(empfehlung)
                )
      if (!is.null(details)){
                 page<-list(page,tags$a("Details", href=details))
      }
      return(page)
    }
    
      
    # create table in variable res
    
    
    res <- renderContracts(versicherung$vertraegeTabelle)
    
    # create content with recommendations and tips
    if (type=="Haftpflicht"){      
    
      #### Calculation Haftpflicht
      if (nrow(versicherung$vertraegeTabelle)>1){
        res<-list(res
                  ,renderPage(absicherung="überversichert"
                              ,empfehlung="Jeder Haushalt benötigt nur eine Haftpflichtversicherung. Melden Sie einer Versicherung alle Personen des Haushalts und kündigen Sie die andere."
                            )
                  )
      }else{
        if (nrow(versicherung$vertraegeTabelle)<1){
          res<-list(res
                    ,renderPage(absicherung="keine"
                                ,empfehlung="Schließe eine private Haftpflichtversicherung ab."
                    )
          )
        }else{
          res<-list(res
                    ,renderPage(absicherung="ok"
                                ,empfehlung="Nichts zu tun."
                    )
          )
        }
      }
    }
    
    ### Calcualtion BU
    
    if (type=="Invaliditaet"){
      res<-list(res
                ,renderPage(absicherung="minimal"
                            ,empfehlung="Informiere Dich über den Abschluß einer Beruftsunfähigkeitsversicherung (kurz BU) oder einer Unfallversicherung."
                            ,details="http://cloud.thetaris.com/activateReport?report=1510&active=true"
                )
      )
    }
    return(res)
  }
  )
}
