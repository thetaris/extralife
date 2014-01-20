library(shiny)
library(rCharts)
source('../common/getELTYPE.R', encoding="UTF-8")
source('../common/getELFIELD.R')
source('../common/readDGSData.R')

source("computations.R", encoding="UTF-8")
source("renderDetail.R", encoding="UTF-8")
#eval(parse("computations.R", encoding="UTF-8"))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  
  dataObj = isolate(DGSData(session=session))    
  
  versicherungen <- getVersicherungen(dataObj)
  besitz <- getBesitz(dataObj)
  familie <- getFamilie(dataObj)
  
  for (iterContracts in names(versicherungen)){
    #does not work in shiny:
    #output[iterContracts] <- renderDetail(versicherungen[iterContracts])    

    # -> build string and eval command
    cmdStr <- sprintf("output$%s <- renderDetail(versicherungen, besitz, familie, input, '%s')",  iterContracts, iterContracts)
    eval(parse(text=cmdStr))    
  }
    
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
    versicherungen <- getEmpfehlungen(versicherungen, besitz, familie, input)
        
    tmp = list(tags$th(tags$h4("Risiko")), tags$th(tags$h4("möglicher Schaden")), tags$th(tags$h4("Abdeckung")), tags$th(tags$h4("Status")))    
    res = tags$tr(tmp)
    
    for (iterRisk in names(versicherungen))
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
         
  }
  )
  
})