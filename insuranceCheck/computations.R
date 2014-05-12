

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
  versicherungen$Invaliditaet        = getData(type=c(ELTYPE$Unfallversicherung
                                                      , ELTYPE$Berufsunfaehigkeitsversicherung
                                                      , ELTYPE$Private.Erwerbsunfaehigkeitsversicherung))
  versicherungen$Tod                 = getData(type=ELTYPE$Lebensversicherung._)
  versicherungen$SchadenAmAuto       = getData(type=ELTYPE$KFZ.Kaskoversicherung)
  versicherungen$KFZHaftpflicht      = getData(type=ELTYPE$KFZ.Haftpflichtversicherung)
  versicherungen$SchadenAmEigentum   = getData(type=ELTYPE$Hausratversicherung)
  versicherungen$Rechtsstreit        = getData(type=c( ELTYPE$Rechtsschutzversicherung, ELTYPE$Berufsverband ))
  
  return(versicherungen)
}

getBesitz <- function(dataObj){
  besitz <- list()
  besitz$title    <- dataObj$get(requestedField = ELFIELD$title, type=ELTYPE$Mein.Besitz._)
  besitz$zeitwert <- as.numeric(dataObj$get(requestedField = ELFIELD$i.wert, type=ELTYPE$Mein.Besitz._))
  besitz$node_id  <- as.numeric(dataObj$get(requestedField = ELFIELD$node_id, type=ELTYPE$Mein.Besitz._))
  besitz$type     <- as.numeric(dataObj$get(requestedField = ELFIELD$type_id, type=ELTYPE$Mein.Besitz._))
  return(besitz)
}

getFamilie <- function(dataObj){
  familie = list()
  familie$name <- dataObj$get(requestedField = ELFIELD$title, type=ELTYPE$Meine.Familie._)
  familie$rel  <- dataObj$get(requestedField = ELFIELD$type_id, type=ELTYPE$Meine.Familie._)
  
  return(familie)
}

getEmpfehlungen <- function(versicherungen, besitz, familie, input){
  # test for empty data and display message if empty using simpson test data
  
  if (length(familie)==0) {
    dataObj = isolate(DGSData(file = "../test/data/test_simpson.json"))
    versicherungen <- getVersicherungen(dataObj)
    besitz <- getBesitz(dataObj)
    familie <- getFamilie(dataObj)
    
    recom <- getEmpfehlungen(versicherungen, besitz, familie, list("variable"="wenig"))  
    
    for (iter in names(recom)){
      recom[[iter]]$absicherung <- "Gibt bitte Deine Daten ein."
      recom[[iter]]$abdeckung <- 0
      recom[[iter]]$empfehlung <-  "Gibt bitte Deine Daten ein."
      recom[[iter]]$status <- "red"
      #recom$haftpflicht$schaden <- 8
    }
    return(recom)
  }
  
  
  # API for HTML renderer requires status strings
  StatusColors = list(grau="grey", gelb="yellow", gruen="green", rot="red")
  
  renderInfoHTML <- function(url, text, cosmos=NULL){
    
    infoHTML = list()
    
    for (iterUrl in c(1:length(url))){
      infoHTML = list( infoHTML
        , list( tags$a(text[iterUrl], href=url[iterUrl], target="_blank") )
        , tags$br()        
        )
      }
    if (!is.null(cosmos)){
      infoHTML = list(infoHTML, cosmos)
    }
    
    return(infoHTML)
  }
  
  
  ####### Recommendations
  # Haftpflicht
  sel = nrow(versicherungen$haftpflicht$vertraegeTabelle)
  if (sel == 0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Schließe eine private Haftpflichtversicherung ab."
    status = StatusColors$rot
  } else if (sel == 1){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
    status = StatusColors$gruen
  } else if (sel>1){
    absicherung = "überversichert"
    abdeckung = 5
    empfehlung = "Jeder Haushalt benötigt nur eine Haftpflichtversicherung. 
    Melde einer Deiner Versicherungen alle Personen des Haushalts und kündige die andere."
    status = StatusColors$gelb
  }  
  versicherungen$haftpflicht$absicherung = absicherung
  versicherungen$haftpflicht$abdeckung   = abdeckung
  versicherungen$haftpflicht$empfehlung  = empfehlung
  
  versicherungen$haftpflicht$link  = "linkToPrivHaftpflicht"
  versicherungen$haftpflicht$titel  = "private Haftpflicht"
  versicherungen$haftpflicht$status  = status
  versicherungen$haftpflicht$schaden = 8
  
  url = "https://www.bundderversicherten.de/files/bulletins/pdf/44_M_PH_NMG.pdf"
  text = "Merkblatt Haftpflichtversicherung"  
  cosmos= list(tags$a("gut (04/2010): CosmosDirekt",href="http://ad.zanox.com/ppc/?26999506C810390156T&ULP=[[XXX]]", target="_blank")
                                                   ,tags$img(src="http://ad.zanox.com/ppv/?26999506C810390156", align="bottom", width="1", height="1", border="0", hspace="1"))
  versicherungen$haftpflicht$infoHTML = renderInfoHTML(url, text, cosmos)
  
  # Krankheit 
  sel = nrow(versicherungen$Krankheit$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine"
    abdeckung = 0
    empfehlung = "Jeder sollt eine Krankenversicherung haben, schließe bitte eine gesetzliche oder private Krankenversicherung ab."
    status = StatusColors$rot
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 4
    empfehlung = "Nichts zu tun."
    status = StatusColors$gruen
  }           
  versicherungen$Krankheit$absicherung = absicherung
  versicherungen$Krankheit$empfehlung  = empfehlung
  versicherungen$Krankheit$abdeckung   = abdeckung
  
  versicherungen$Krankheit$link  = "linkToKrankheit"
  versicherungen$Krankheit$titel  = "Krankheit"
  versicherungen$Krankheit$status  = status
  versicherungen$Krankheit$schaden = 6
  url = c("https://www.bundderversicherten.de/files/bulletins/pdf/39_M_Wechsel_GKV.pdf"
                                           ,"https://www.bundderversicherten.de/downloads/broschueren/BdV_PKV-Broschuere.pdf"
                                           ,"https://www.bundderversicherten.de/files/bulletins/pdf/52_M_PKV_Zusatz_NMG.pdf"
  )
  text = c("Merkblatt gesetzliche Krankenvollversicherung",
                                            "Merkblatt private Krankenvollversicherung",
                                            "Merkblatt private Krankenzusatzversicherung")
  
  versicherungen$haftpflicht$infoHTML = renderInfoHTML(url, text, cosmos=NULL)
  
  
  # Invaliditaet
  sel = nrow(versicherungen$Invaliditaet$vertraegeTabelle)
  if (sel==0){
    # keinen Partner, keine Kinder, wenig Absicherung
    if ((input$variable=="wenig") & (!((familie$rel %in% ELTYPE$Kinder._)|(familie$rel %in% ELTYPE$Partner._)))){
      absicherung = "ok"
      abdeckung = 0
      empfehlung = "Du bist nicht gut für den Fall einer chronischen Erkrankung vorbereitet. 
      Im Notfall bist Du vermutlich auf staatliche Unterstützung angewiesen und kannst Deinen 
      Lebensstandart nicht halten. Da Du aber für Dich selbst verantwortlich bist und keine Kinder 
      oder Partner zu versorgen hast, kannst Du diese Versicherung einsparen."      
      status = StatusColors$grau
    }
    else{
      
      absicherung = "keine"
      abdeckung = 0
      empfehlung = "Jeder sollte für den Fall der Invalidität vorsorgen. Bitte schließe eine Berufsunfähigkeitsversicherung (BU) ab."#
      status = StatusColors$rot
    }
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."    
    status = StatusColors$gruen
  } 
  detailsURL = "/activateReport?report=1510&active=true"
  
  versicherungen$Invaliditaet$absicherung = absicherung
  versicherungen$Invaliditaet$abdeckung   = abdeckung
  versicherungen$Invaliditaet$detailsURL  = detailsURL
  versicherungen$Invaliditaet$empfehlung  = empfehlung
  
  versicherungen$Invaliditaet$link  = "linkToInvaliditaet"
  versicherungen$Invaliditaet$titel  = "Invalidität"
  versicherungen$Invaliditaet$status  = status
  versicherungen$Invaliditaet$schaden = 7
  
  url = c("https://www.bundderversicherten.de/files/bulletins/pdf/97_M_BU_NMG.pdf"
                                              ,"https://www.bundderversicherten.de/files/bulletins/pdf/68_M_U_NMG.pdf")
  text = c("Merkblatt BU"
                                               ,"Merkblatt Unfallversicherung")

  
  versicherungen$Invaliditaet$infoHTML = renderInfoHTML(url, text)
  
  #  Tod
  sel = nrow(versicherungen$Tod$vertraegeTabelle)
  if (sel==0){
    absicherung = "keine"
    abdeckung = 0
    
    if (sum(familie$rel %in% ELTYPE$Kinder._)){
      if (sum(familie$rel %in% ELTYPE$Partner._)){
        empfehlung = "Du solltest eine Risikolebensversicherung abschließen um Deinen Partner und Deine Kinder im Notfall abzusichern."
        status = StatusColors$rot
        
      }else{
        empfehlung = "Du solltest eine Risikolebensversicherung abschließen um Deine Kinder im Notfall abzusichern." 
        status = StatusColors$rot
      }            
    } else if (sum(familie$rel %in% ELTYPE$Partner._)){
      empfehlung = "Du solltest eine Risikolebensversicherung abschließen um Deinen Partner im Notfall abzusichern."
      status = StatusColors$gelb
    } else {
      absicherung = "ok"
      abdeckung = 0
      empfehlung = "Nichts zu tun."
      status = StatusColors$grau
    }    
    
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
    status = StatusColors$gruen
  }          
  versicherungen$Tod$absicherung = absicherung
  versicherungen$Tod$abdeckung   = abdeckung
  versicherungen$Tod$empfehlung  = empfehlung
  
  versicherungen$Tod$link  = "linkToTod"
  versicherungen$Tod$titel  = "Tod"
  versicherungen$Tod$status  = status
  versicherungen$Tod$schaden = 7
  url = "https://www.bundderversicherten.de/files/bulletins/pdf/95_M_RLV_NMG.pdf"
  text = "Merkblatt Risikolebensversicherung"
  cosmos = list(tags$a("Testsieger (4/2013): CosmosDirekt",href="http://ad.zanox.com/ppc/?26999293C615999878T&ULP=[[XXX]]", target="_blank")
                                           ,tags$img(src="http://ad.zanox.com/ppv/?26999293C615999878", align="bottom", width="1", height="1", border="0", hspace="1")
  )
  
  versicherungen$Tod$infoHTML = renderInfoHTML(url, text, cosmos)
  
  # SchadenAmAuto
  sel = nrow(versicherungen$SchadenAmAuto$vertraegeTabelle)
  if (sel<sum(besitz$type %in% ELTYPE$Automobil)){    
    
    abdeckung = round(5*sel/sum(besitz$type %in% ELTYPE$Automobil))
    if (is.na(abdeckung)){
      abdeckung = 0
    }
    
    if (input$variable == "wenig"){
      absicherung = "ok"
      
      empfehlung = "Eine Kaskoversicherung für Dein Auto ist nicht unbedingt notwendig. Hier brauchst Du nichts tun."      
      status = StatusColors$grau
    } else {
      absicherung = "unzureichend"
      
      empfehlung = "Eine Kaskoversicherung ist für einen umfassenden Versicherungschutz wichtig. Du solltest eine KFZ Kaskoversicherung abschließen."            
      status = StatusColors$gelb
    }
    
  } else if (sel==sum(besitz$type %in% ELTYPE$Automobil)){
    if (!any(besitz$type %in% ELTYPE$Automobil)){      
      absicherung = "ok"
      abdeckung = 0
      empfehlung = "Nichts zu tun."
      status = StatusColors$grau
    }else{
      absicherung = "ok"
      abdeckung = 5
      empfehlung = "Nichts zu tun."      
      status = StatusColors$gruen
    }
  } else if (sel>sum(besitz$type %in% ELTYPE$Automobil)){
    absicherung = "überversichert."
    abdeckung = 5
    empfehlung = "was ist da passiert? Du hast mehr Kaskoversicherungen eingetragen als Autos."    
    status = StatusColors$gelb
  }
  versicherungen$SchadenAmAuto$absicherung = absicherung
  versicherungen$SchadenAmAuto$abdeckung   = abdeckung
  versicherungen$SchadenAmAuto$empfehlung  = empfehlung
  
  versicherungen$SchadenAmAuto$link  = "linkToSchadenamAuto"
  versicherungen$SchadenAmAuto$titel  = "Schaden am Auto"
  versicherungen$SchadenAmAuto$status  = status    
  
  tmp = besitz$zeitwert[besitz$type %in% ELTYPE$Automobil]
  
  if (length(tmp>0)){
    schaden = ceiling(log(sum(tmp))/log(10))
  }else{
    schaden = 0
  }
  
  versicherungen$SchadenAmAuto$schaden =  schaden
  url = c("https://www.bundderversicherten.de/files/bulletins/pdf/50_M_Kfz_NMG.pdf"
  )
  text = c("Merkblatt KFZ Versicherung"
  )
  
  versicherungen$SchadenAmAuto$infoHTML = renderInfoHTML(url, text)
  
  # KFZHaftpflicht
  schaden=8
  
  sel = nrow(versicherungen$KFZHaftpflicht$vertraegeTabelle)
  if (sel<sum(besitz$type %in% ELTYPE$Automobil)){    
    abdeckung = round(5*sel/sum(besitz$type %in% ELTYPE$Automobil))
    if (is.na(abdeckung)){
      abdeckung = 0
    }
    absicherung = "unzureichend"
    
    empfehlung = "Eine KFZ Haftpflichtversicherung ist für jedes Auto zwingend vorgeschrieben. Bitte schließe eine KFZ Haftpflichtversicherung ab."            
    status = StatusColors$rot
    
  } else if (sel==sum(besitz$type %in% ELTYPE$Automobil)){
    if (!any(besitz$type %in% ELTYPE$Automobil)){      
      absicherung = "ok"
      abdeckung = 0
      empfehlung = "Nichts zu tun."
      schaden=0
      status=StatusColors$grau
    }else{
      absicherung = "ok"
      abdeckung = 5
      empfehlung = "Nichts zu tun."
      status = StatusColors$gruen
    }
  } else if (sel>sum(besitz$type %in% ELTYPE$Automobil)){
    absicherung = "überversichert."
    abdeckung = 5
    empfehlung = "was ist da passiert? Du hast mehr KFZ Haftpflichtversicherungen eingetragen als Autos."    
    status = StatusColors$gelb
  }
  
  versicherungen$KFZHaftpflicht$absicherung = absicherung
  versicherungen$KFZHaftpflicht$abdeckung   = abdeckung
  versicherungen$KFZHaftpflicht$empfehlung  = empfehlung
  
  versicherungen$KFZHaftpflicht$link  = "linkToKFZHaftpflicht"
  versicherungen$KFZHaftpflicht$titel  = "KFZ Haftpflicht"
  versicherungen$KFZHaftpflicht$status  = status
  versicherungen$KFZHaftpflicht$schaden =  schaden
  
  url = c("https://www.bundderversicherten.de/files/bulletins/pdf/50_M_Kfz_NMG.pdf")  
  text = c("Merkblatt KFZ Versicherung")
  versicherungen$KFZHaftpflicht$infoHTML = renderInfoHTML(url, text)
  
  
  # SchadenAmEigentum
  sel=nrow(versicherungen$SchadenAmEigentum$vertraegeTabelle)
  if (sel==0){
    if ((input$variable=="wenig")|(input$variable=="mittel")){
      absicherung = "ok"
      abdeckung = 0
      empfehlung = "Du hast keine Hausratsversicherung eingetragen. Diese Versicherung ist nicht zwingend notwendig und Du kannst sie einsparen."      
      status = StatusColors$grau
    }else{
      absicherung = "keine"
      abdeckung = 0
      empfehlung = "Du hast keine Versicherung eingetragen. Um einem guten Versicherungsschutz zu erreichen solltest Du eine Hausratsversicherung abschließen."
      status = StatusColors$gelb
    }
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
    status = StatusColors$gruen
  }

  tmp = besitz$zeitwert[besitz$type %in% ELTYPE$Hausrat._]
  
  if (length(tmp>0)){
    schaden = ceiling(log(sum(tmp[!is.na(tmp)]))/log(10))
    if (schaden==0){
      if (abdeckung == 0){
      empfehlung = "Du hast kein Eigentum mit Wert eingetragen. Daher scheinst Du auch keine Hausratsversicherung zu benötigen. Trage ggf. Dein Eigentum noch ein."
      status = StatusColors$grau      
      }else{
        empfehlung = "Du hast kein Eigentum mit Wert aber eine Hausratsversicherung eingetragen. Du scheinst keine Hausratsversicherung zu benötigen. Trage ggf. Dein Eigentum noch ein oder kündige die Versicherung."
        status = StatusColors$gelb              
      }
    }
  }else{
    schaden = 0
    empfehlung = "Du hast kein Eigentum eingetragen. Daher scheinst Du auch keine Hausratsversicherung zu benötigen. Trage ggf. Dein Eigentum noch ein."
    status = StatusColors$grau
  }
  
  
  versicherungen$SchadenAmEigentum$absicherung = absicherung
  versicherungen$SchadenAmEigentum$abdeckung   = abdeckung
  versicherungen$SchadenAmEigentum$empfehlung  = empfehlung
  
  versicherungen$SchadenAmEigentum$link  = "linkToSchadenamEigentum"
  versicherungen$SchadenAmEigentum$titel  = "Schaden am Eigentum"
  versicherungen$SchadenAmEigentum$status  = status
  

  versicherungen$SchadenAmEigentum$schaden =  schaden
  
  url = c("https://www.bundderversicherten.de/files/bulletins/pdf/46_M_H_NMG.pdf")
  text = c("Merkblatt Hausratsversicherung")  
  versicherungen$SchadenAmEigentum$infoHTML = renderInfoHTML(url, text)
  
  
  # Rechtsstreit
  sel=nrow(versicherungen$Rechtsstreit$vertraegeTabelle)
  if (sel==0){
    if ((input$variable=="wenig")|(input$variable=="mittel")){
      absicherung = "ok"
      abdeckung = 0
      empfehlung = "Du hast keine Rechtsschutzversicherung eingetragen. Diese Versicherung ist 
                    nicht zwingend notwendig und Du kannst sie einsparen."    
      status = StatusColors$grau
    }else{
      absicherung = "keine"
      abdeckung = 0
      empfehlung = "Um einem guten Versicherungsschutz zu erreichen solltest Du eine Rechtsschutzversicherung abschließen."
      status = StatusColors$gelb
    }
  } else if (sel>0){
    absicherung = "ok"
    abdeckung = 5
    empfehlung = "Nichts zu tun."
    status = StatusColors$gruen
  }
  versicherungen$Rechtsstreit$absicherung = absicherung
  versicherungen$Rechtsstreit$abdeckung   = abdeckung
  versicherungen$Rechtsstreit$empfehlung  = empfehlung
  
  versicherungen$Rechtsstreit$link  = "linkToRechtsstreit"
  versicherungen$Rechtsstreit$titel  = "Rechtsstreit"
  versicherungen$Rechtsstreit$status  = status
  versicherungen$Rechtsstreit$schaden =  5
  
  url = c("https://www.bundderversicherten.de/files/bulletins/pdf/58_M_R_NMG.pdf")
  text = c("Merkblatt Rechtsschutzversicherung")  
  versicherungen$Rechtsstreit$infoHTML = renderInfoHTML(url, text)
  
  return(versicherungen) 
}

