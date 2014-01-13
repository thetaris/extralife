getVersicherungen <- function(dataObj){
  getData<- function(type)  {
    tmpVertrag = list()
    tmp.titel = dataObj$get(type=type, requestedField=ELFIELD$title)  
    tmp.ich = dataObj$get(type=ELTYPE$Ich, requestedField=ELFIELD$title)  
    
    tmpVertrag$vertraegeTabelle = data.frame(Vertragsname=tmp.titel, Versicherungsnehmer = rep(tmp.ich,length(tmp.titel)))
    return(tmpVertrag)
  }
  
  versicherungen <- list()
  
  # private Haftpflicht
  haftpflicht <- list()
 
  versicherungen$haftpflicht         = getData(type=ELTYPE$Privathaftpflichtversicherung)  
  versicherungen$Krankheit           = getData(type=ELTYPE$Krankenversicherung._)
  versicherungen$Invaliditaet        = getData(type=ELTYPE$Invaliditätsversicherung._)
  versicherungen$Tod                 = getData(type=ELTYPE$Lebensversicherung._)
  versicherungen$SchadenAmAuto       = getData(type=ELTYPE$KFZ.Kaskoversicherung)
  versicherungen$KFZHaftpflicht      = getData(type=ELTYPE$KFZ.Haftpflichtversicherung)
  versicherungen$SchadenAmEigentum   = getData(type=ELTYPE$Hausratversicherung)
  versicherungen$Rechtsstreit        = getData(type=c( ELTYPE$Rechtsschutzversicherung, ELTYPE$Berufsverband ))
  return(versicherungen)
}

renderDetail <- function(versicherung, type){
  renderUI({
    result<-list()
    
    result<-list(result,tags$h3(sprintf("vorhandene Verträge:")))
    
    # create table in variable res
    tmp <- list(tags$th(tags$h4("Vertrag")), tags$th(tags$h4("Versicherungsnehmer")), tags$th(tags$h4("monatl. Kosten")))    
    res <- tags$tr(tmp)
    
    if (nrow(versicherung$vertraegeTabelle)>0){
      for (iterVertrag in 1:nrow(versicherung$vertraegeTabelle))
      {
        tmp <- list(tags$th(versicherung$vertraegeTabelle[iterVertrag, "Vertragsname"], align="left")
                    ,tags$th(versicherung$vertraegeTabelle[iterVertrag, "Versicherungsnehmer"], align="right")
                    ,tags$th("50 EUR")    
        )            
        res <- list(res, tags$tr(tmp))
      }     
    }else{
      res <- list(res, tags$tr(tags$th("keine")))
    }
    res <- tags$table(res, rules="rows", cellpadding="10%", align="center")
    
    res <- list(result, res)
    
    if (type=="Haftpflicht"){
      
      haftpflicht <- versicherung
      
      #### Calculation Haftpflicht
      if (nrow(haftpflicht$vertraegeTabelle)>1){
        res<-list(res,tags$h3(sprintf("Absicherung: %s", "überversichert")))
        res<-list(res,tags$p("Empfehlung: Jeder Haushalt benötigt nur eine Haftpflichtversicherung. Melden Sie einer Versicherung alle Personen des Haushalts und kündigen Sie die andere."))      
      }else{
        if (nrow(haftpflicht$vertraegeTabelle)<1){
          res<-list(res,tags$h3(sprintf("Absicherung: %s", "keine")))
          res<-list(res,tags$p("Empfehlung: Schließe eine private Haftpflichtversicherung ab."))      
        }else{
          res<-list(res,tags$h3(sprintf("Absicherung: %s", "ok")))
          res<-list(res,tags$p("Empfehlung: Nichts zu tun."))      
        }
      }
    }
    if (type=="Invaliditaet"){
      res = list(res
      , tags$h3(sprintf("Absicherung: %s", "minimal"))
      , tags$p("Empfehlung: Informiere Dich über den Abschluß einer Beruftsunfähigkeitsversicherung (kurz BU) oder einer Unfallversicherung.")     
      , tags$a("Details", href="http://cloud.thetaris.com/activateReport?report=1510&active=true")
      )
    }
    return(res)
  }
  )
}
