getHaftpflicht <- function(dataObj){

haftpflicht <- list()
tmp.titel = dataObj$get(type=ELTYPE$Privathaftpflichtversicherung, requestedField=ELFIELD$title)  
tmp.ich = dataObj$get(type=ELTYPE$Ich, requestedField=ELFIELD$title)  

haftpflicht$vertraegeTabelle = data.frame(Vertragsname=tmp.titel, Versicherungsnehmer = tmp.ich)

return(haftpflicht)
}

getOverview <- function(dataObj){
  Deine_wichtigsten_Risiken = c('<a href="krankheit">Krankheit</a>',
                                '<a href="invaliditaet">Invalidit&auml;t</a>',
                                '<a href="tod">Tod</a>',
                                '<a href="autoschaden">Schaden am Auto</a>',
                                #                                  '<a href="privateHaftpflicht">private Haftpflicht</a>',
                                '<div id="linkToPrivHaftpflicht"> 
                                   <a>private Haftpflicht</a>
                                 </div>',
                                '<a href="haftpflichtprivat">KFZ Haftpflicht</a>',
                                '<a href="eigentumsschaden">Schaden am Eigentum</a>',
                                '<a href="rechtsstreit">Rechtsstreit</a>'
  )
  
  
  Geldbedarf_bei_Schaden = c('&euro;.&euro;&euro;&euro;.&euro;&euro;&euro;',
                             '&euro;.&euro;&euro;&euro;.&euro;&euro;&euro;',
                             '&euro;&euro;&euro;.&euro;&euro;&euro;',
                             '&euro;&euro;.&euro;&euro;&euro;',
                             '&euro;.&euro;&euro;&euro;.&euro;&euro;&euro;',
                             '&euro;&euro;.&euro;&euro;&euro;.&euro;&euro;&euro;',
                             '&euro;&euro;.&euro;&euro;&euro;',
                             '&euro;&euro;.&euro;&euro;&euro;'
  )
  
  Abdeckung = c('<img src="http://thetava.com/Shiny_icons/Signal_5.png" width="25"/>',
                '<img src="http://thetava.com/Shiny_icons/Signal_2.png" width="25"/>',
                '<img src="http://thetava.com/Shiny_icons/Signal_1.png" width="25"/>',
                '<img src="http://thetava.com/Shiny_icons/Signal_3.png" width="25"/>',
                '<img src="http://thetava.com/Shiny_icons/Signal_5.png" width="25"/>',
                '<img src="http://thetava.com/Shiny_icons/Signal_5.png" width="25"/>',
                '<img src="http://thetava.com/Shiny_icons/Signal_0.png" width="25"/>',
                '<img src="http://thetava.com/Shiny_icons/Signal_0.png" width="25"/>'
  )
  
  Status = c('<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>',
             '<div style="display:none">0</div> <img src="http://thetava.com/Shiny_icons/Warning_Icon_32.png"/>',
             '<div style="display:none">0</div> <img src="http://thetava.com/Shiny_icons/Warning_Icon_32.png"/>',
             '<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>',
             '<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>',
             '<div style="display:none">0</div> <img src="http://thetava.com/Shiny_icons/Warning_Icon_32.png"/>',           
             '<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>',
             '<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>'
  )
  
  overview <- list()
  overview$table <- data.frame(Deine_wichtigsten_Risiken, Geldbedarf_bei_Schaden, Abdeckung, Status)
  
  return(overview)
}