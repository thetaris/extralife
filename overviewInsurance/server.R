library(shiny)
library(rCharts)
source('../common/getELTYPE.R')
source('../common/getELFIELD.R')
source('../common/readDGSData.R')

source("computations.R", encoding="UTF-8")
#eval(parse("computations.R", encoding="UTF-8"))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  
  dataObj = isolate(DGSData(session=session))    
  
  versicherungen <- getVersicherungen(dataObj)
  
  
  output$haftpflicht <- renderDetail(versicherungen$haftpflicht, "Haftpflicht")
  output$Krankheit   <- renderDetail(versicherungen$Krankheit, "Krankheit")
  output$Invaliditaet <- renderDetail(versicherungen$Invaliditaet, "Invaliditaet")
  output$Tod <- renderDetail(versicherungen$Tod, "Tod")  
  output$SchadenAmAuto <- renderDetail(versicherungen$SchadenAmAuto, "SchadenAmAuto")
  output$KFZHaftpflicht <- renderDetail(versicherungen$KFZHaftpflicht, "KFZHaftpflicht")
  output$SchadenAmEigentum <- renderDetail(versicherungen$SchadenAmEigentum, "SchadenAmEigentum")
  output$Rechtsstreit <- renderDetail(versicherungen$Rechtsstreit, "Rechtsstreit")
  
  output$overviewTable <- renderUI({
    renderSchaden <- function(logSum){
      symbol = tags$img(src="http://thetava.com/Shiny_icons/euro.png", width="25", border="0")
      space = tags$a(".")
      
      res = list();
      for (iter in 1:logSum){
        res = list(symbol, res)        
        if ( ((iter %% 3) == 0) & (iter<logSum) ){
          res = list(space, res)
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
    
    link = c("linkToPrivHaftpflicht", "linkToKrankheit","linkToInvaliditaet","linkToTod", "linkToSchadenamAuto", "linkToKFZHaftpflicht", "linkToSchadenamEigentum", "linkToRechtsstreit")
    titel = c("private Haftpflicht", "Krankheit","Invalidität","Tod", "Schaden am Auto", "KFZ Haftpflicht", "Schaden am Eigentum", "Rechtsstreit")
    schaden = c(8, 6, 7, 7, 6, 8, 5, 5)
    abdeckung = c(5, 5, 1, 1, 5, 5, 3, 0)
    status = c(0,0,1,1,0,0,0,0)
    
    #### Calculation Haftpflicht
    if (nrow(haftpflicht$vertraegeTabelle)>1){
      status[1]<- 1
    }
    if (nrow(haftpflicht$vertraegeTabelle)<1){
      status[1]<- 1
    }
    
    uebsersichtTable <- data.frame(link, titel, schaden, abdeckung, status)
    
    tmp = list(tags$th(tags$h4("Risiko")), tags$th(tags$h4("möglicher Schaden")), tags$th(tags$h4("Abdeckung")), tags$th(tags$h4("Status")))    
    res = tags$tr(tmp)
    
    for (iterRisk in 1:nrow(uebsersichtTable))
    {
      tmp = list(tags$th(div(id= uebsersichtTable[iterRisk, "link"]
                             ,tags$a( uebsersichtTable[iterRisk, "titel"])), align="left")
                ,tags$th(renderSchaden( uebsersichtTable[iterRisk, "schaden"]), align="right")
                ,tags$th(renderAbdeckung( uebsersichtTable[iterRisk, "abdeckung"]))
                ,tags$th(renderStatus( uebsersichtTable[iterRisk, "status"])))    
      
      res = list(res, tags$tr(tmp))
    }              
    res = tags$table(res, rules="rows", cellpadding="5%", align="center")
      
    scripts = list()
    for (iterRisk in 1:nrow(uebsersichtTable))
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
        			", uebsersichtTable[iterRisk, "link"],iterRisk, iterRisk, uebsersichtTable[iterRisk, "link"])))
    }
    res = list(res,tags$script(scripts))
         
  }
  )
  
})