source("../common/utilEL.R", encoding="UTF-8")

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
  
  ######## Tables with Contracts
  
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

getBesitz <- function(dataObj){
  besitz <- list()
  besitz$title    <- dataObj$get(requestedField = ELFIELD$title, type=ELTYPE$Mein.Besitz)
  besitz$zeitwert <- dataObj$get(requestedField = ELFIELD$zeitwert.betrag, type=ELTYPE$Mein.Besitz)
  besitz$node_id  <- dataObj$get(requestedField = ELFIELD$node_id, type=ELTYPE$Mein.Besitz)
  
  return(besitz)
}

getEmpfehlungen <- function(versicherungen, besitz, input){
  
  ####### Recommendations
  # Haftpflicht
  sel = nrow(versicherungen$haftpflicht$vertraegeTabelle)
  if (sel == 0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Schließe eine private Haftpflichtversicherung ab."
  } else if (sel == 1){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
  } else if (sel>1){
    absicherung = "überversichert"
    abdeckung = 5
    empfehlung = "Jeder Haushalt benötigt nur eine Haftpflichtversicherung. Melden Sie einer Versicherung alle Personen des Haushalts und kündigen Sie die andere."
  }  
  versicherungen$haftpflicht$absicherung = absicherung
  versicherungen$haftpflicht$abdeckung   = abdeckung
  versicherungen$haftpflicht$empfehlung  = empfehlung
  
  versicherungen$haftpflicht$link  = "linkToPrivHaftpflicht"
  versicherungen$haftpflicht$titel  = "private Haftpflicht"
  versicherungen$haftpflicht$status  = switch(absicherung,ok=0,1)
  versicherungen$haftpflicht$schaden = 8
  versicherungen$haftpflicht$linkBdV$URL = "https://www.bundderversicherten.de/Haftpflicht"
  versicherungen$haftpflicht$linkBdV$text = "Private Haftpflicht"
  versicherungen$haftpflicht$linkCosmos$HTML= list(tags$a("gut (04/2010): CosmosDirekt",href="http://ad.zanox.com/ppc/?26999506C810390156T&ULP=[[XXX]]", target="_blank")
                                           ,tags$img(src="http://ad.zanox.com/ppv/?26999506C810390156", align="bottom", width="1", height="1", border="0", hspace="1")
  )
  
    
  # Krankheit
  sel = nrow(versicherungen$Krankheit$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Sie haben keine Krankenversicherung eingetragen. Jeder sollt eine Krankenversicherung haben."
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 4
    empfehlung = "Nichts zu tun."
  }           
  versicherungen$Krankheit$absicherung = absicherung
  versicherungen$Krankheit$empfehlung  = empfehlung
  versicherungen$Krankheit$abdeckung   = abdeckung
  
  versicherungen$Krankheit$link  = "linkToKrankheit"
  versicherungen$Krankheit$titel  = "Krankheit"
  versicherungen$Krankheit$status  = switch(absicherung,ok=0,1)
  versicherungen$Krankheit$schaden = 6
  
  # Invaliditaet
  sel = nrow(versicherungen$Invaliditaet$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Sie haben keine Versicherung eingetragen. Jeder sollt eine Versicherung haben."#
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."    
  } 
  detailsURL = "/activateReport?report=1510&active=true"
  
  versicherungen$Invaliditaet$absicherung = absicherung
  versicherungen$Invaliditaet$abdeckung   = abdeckung
  versicherungen$Invaliditaet$detailsURL  = detailsURL
  versicherungen$Invaliditaet$empfehlung  = empfehlung
  
  versicherungen$Invaliditaet$link  = "linkToInvaliditaet"
  versicherungen$Invaliditaet$titel  = "Invalidität"
  versicherungen$Invaliditaet$status  = switch(absicherung,ok=0,1)
  versicherungen$Invaliditaet$schaden = 7
  
  #  Tod
  sel = nrow(versicherungen$Tod$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Sie haben keine Versicherung eingetragen. Jeder sollt eine Versicherung haben."
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
  }          
  versicherungen$Tod$absicherung = absicherung
  versicherungen$Tod$abdeckung   = abdeckung
  versicherungen$Tod$empfehlung  = empfehlung
  
  versicherungen$Tod$link  = "linkToTod"
  versicherungen$Tod$titel  = "Tod"
  versicherungen$Tod$status  = switch(absicherung,ok=0,1)
  versicherungen$Tod$schaden = 7
  versicherungen$Tod$linkBdV$URL = "https://www.bundderversicherten.de/Lebensversicherung/Risikoleben"
  versicherungen$Tod$linkBdV$text = "Risikolebensversicherung"
  versicherungen$Tod$linkCosmos$HTML= list(tags$a("Testsieger (4/2013): CosmosDirekt",href="http://ad.zanox.com/ppc/?26999293C615999878T&ULP=[[XXX]]", target="_blank")
                                          ,tags$img(src="http://ad.zanox.com/ppv/?26999293C615999878", align="bottom", width="1", height="1", border="0", hspace="1")
                                                  )
  
  # SchadenAmAuto
  sel = nrow(versicherungen$SchadenAmAuto$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine";
    abdeckung = 0
    empfehlung = "Sie haben keine Versicherung eingetragen. Jeder sollt eine Versicherung haben."
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
  }
  versicherungen$SchadenAmAuto$absicherung = absicherung
  versicherungen$SchadenAmAuto$abdeckung   = abdeckung
  versicherungen$SchadenAmAuto$empfehlung  = empfehlung
  
  versicherungen$SchadenAmAuto$link  = "linkToSchadenamAuto"
  versicherungen$SchadenAmAuto$titel  = "Schaden am Auto"
  versicherungen$SchadenAmAuto$status  = switch(absicherung,ok=0,1)
  versicherungen$SchadenAmAuto$schaden =  6
  
  # KFZHaftpflicht
  sel = nrow(versicherungen$KFZHaftpflicht$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Sie haben keine Versicherung eingetragen. Jeder sollt eine Versicherung haben."
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
  }
  versicherungen$KFZHaftpflicht$absicherung = absicherung
  versicherungen$KFZHaftpflicht$abdeckung   = abdeckung
  versicherungen$KFZHaftpflicht$empfehlung  = empfehlung
  
  versicherungen$KFZHaftpflicht$link  = "linkToKFZHaftpflicht"
  versicherungen$KFZHaftpflicht$titel  = "KFZ Haftpflicht"
  versicherungen$KFZHaftpflicht$status  = switch(absicherung,ok=0,1)
  versicherungen$KFZHaftpflicht$schaden =  8
  
  # SchadenAmEigentum
  sel=nrow(versicherungen$SchadenAmEigentum$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Sie haben keine Versicherung eingetragen. Jeder sollt eine Versicherung haben."#
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
  }
  versicherungen$SchadenAmEigentum$absicherung = absicherung
  versicherungen$SchadenAmEigentum$abdeckung   = abdeckung
  versicherungen$SchadenAmEigentum$empfehlung  = empfehlung
  
  versicherungen$SchadenAmEigentum$link  = "linkToSchadenamEigentum"
  versicherungen$SchadenAmEigentum$titel  = "Schaden am Eigentum"
  versicherungen$SchadenAmEigentum$status  = switch(absicherung,ok=0,1)
  versicherungen$SchadenAmEigentum$schaden =  5
  
  # Rechtsstreit
  sel=nrow(versicherungen$Rechtsstreit$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Sie haben keine Versicherung eingetragen. Jeder sollt eine Versicherung haben."
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 4
    empfehlung = "Nichts zu tun."
  }
  versicherungen$Rechtsstreit$absicherung = absicherung
  versicherungen$Rechtsstreit$abdeckung   = abdeckung
  versicherungen$Rechtsstreit$empfehlung  = empfehlung
  
  versicherungen$Rechtsstreit$link  = "linkToRechtsstreit"
  versicherungen$Rechtsstreit$titel  = "Rechtsstreit"
  versicherungen$Rechtsstreit$status  = switch(absicherung,ok=0,1)
  versicherungen$Rechtsstreit$schaden =  5
  
  return(versicherungen) 
}

renderDetail <- function(versicherung){
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
      page<-list(page,tags$h3(sprintf("Absicherung: %s", absicherung)))
      page<-list(page,tags$h3("Empfehlung")
                 ,tags$p(empfehlung)
      )
      if (!is.null(details)){
        page<-list(page,tags$a("Details", href=details))
      }
      if ((!is.null(linkBdV)) | (!is.null(linkCosmos))){
        page<-list(page,tags$h3("Weiterführende Informationen"))
        if (!is.null(linkBdV)){
          page<-list(page,tags$div("Bund der Versicherten: ", tags$a(linkBdV$text, href=linkBdV$URL, target="_blank")))
        }
        if (!is.null(linkCosmos)){
          page<-list(page,div("Stiftung Warentest: ", linkCosmos$HTML))
          
        }
      }
      return(page)
    }
    
    # create table in variable res    
    
    res <- renderContracts(versicherung$vertraegeTabelle)
    
    res<-list(res
              ,renderPage(versicherung)
    )
    return(res)
  })
}
