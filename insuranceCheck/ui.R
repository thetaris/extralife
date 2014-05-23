detailTab <- function(type, recom_no) {
  div(class ="overview2", 
      div(class="mytable_all", tableOutput(sprintf("mytable_%s",type))),
      div(
        class = "personal", 
        
        div(class = "personalRecom",  div(class="changedata"), 
            div(class = "personalreright",   tags$button(class="graypage-open-datasave", "data-entry-task"="insuranceCheck","VERSICHERUNG HINZUFÜGEN") #actionButton("addinsurancebutton", label = "VERSICHERUNG HINZUFÜGEN"    )
                )
        ),
        
        div(class="warning", htmlOutput(sprintf("warning_%s",type)))
      )
      ,
      
      div(class= "appendix", 
          tabsetPanel(
            tabPanel("EMPFEHLUNG", div(class="empfehlung", uiOutput(sprintf("RecomText_%s",type)))),
            tabPanel(
              "WEITERFÜHRENDE INFORMATIONEN", div(class="weiteinfo", uiOutput(sprintf("weiteinfoText_%s",type)))
            )
          )
      )
  )
  
}

shinyUI(bootstrapPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="overview.css"),
    tags$link(rel="stylesheet", type="text/css", href="details.css"),
    tags$script(src = "action.js", type = "text/javascript"),
    tags$script(src="/sites/all/modules/graypagegridster/actions.js",type="text/javascript")
  ),
  
  tags$div(class="header", list("VERSICHERUNGS", tags$b("ANALYSE"))),
 
  
  
  tabsetPanel(      
        tabPanel(div(class="alloverviewtabs",'ÜBERSICHT'),
                 div(class = "overview", 
                     tableOutput("main_plot"),                        
                     #TODO: recomtext needs to move to the server part and get the content from the recom objetc
                     div(class = "recom",  div(class = "recomPart", div(class ="recomHeadline", "DEINE RISIKOPRÄFERENZ: "), div(class ="recomText", "ICH MÖCHTE MÖGLICHST VIEL ABSICHERN")),                    
                         div(class="changedata", tags$button(class="graypage-open-datasave", "data-entry-task"="insuranceCheck","DATEN ÄNDERN"))
                     )                
                 )             
        ),
        
        tabPanel('HAFTPFLICHT', detailTab("privatehaftpflicht", 1)),   
        tabPanel('KRANKHEIT', detailTab("krankheit", 2)),   
        tabPanel("INVALIDITÄT", detailTab("invaliditaet", 3)), 
        tabPanel("TOD", detailTab("tod", 4)),
        tabPanel("KFZ SCHADEN", detailTab("schadenamauto", 5)),
        tabPanel("KFZ HAFTPFLICHT", detailTab("kfz", 6)), 
        tabPanel("SCHADEN AM EIGENTUM", detailTab("schadenameigentum", 7)),
        tabPanel("RECHTSSTREIT", detailTab("rechtsstreit", 8))    
  )
  , addBugMuncher()
))
