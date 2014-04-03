library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
  
  # Application title
  # headerPanel("Versicherungsanalyse") ,
  
  # Sidebar with a slider input for number of observations
    
    
     
  
  # Show a plot of the generated distribution
  elTags$datasafe.float(task = "1631", text = "Versicherungn hinzufügen"),
  mainPanel(
    tags$table(
      list(tags$td(tags$h2("Versicherungsanalyse "))
           ,tags$td("")      
           ,tags$td(selectInput("variable"," ", choices=list("Möglichst viel absichern" = "viel", 
                     "Ausgewogene Mischung" = "mittel", 
                     "Sparen, zahle kleine Schäden selbst" = "wenig"))
      ))),
    
#    plotOutput("distPlot")
    div(style="width:790px", 
        tabsetPanel(
           tabPanel("Übersicht"
                    , uiOutput("overviewTable")                    
                    )
           ,tabPanel("private Haftpflicht"
                     , uiOutput("haftpflicht")
                     )
           ,tabPanel("Krankheit"
                     , uiOutput("Krankheit")
              )
           ,tabPanel("Invalidität"
                     , uiOutput("Invaliditaet")
           )
           ,tabPanel("Tod"
                     , uiOutput("Tod")
           )
           ,tabPanel("Schaden am Auto"
                     , uiOutput("SchadenAmAuto")
           )
           ,tabPanel("KFZ Haftpflicht"
                     , uiOutput("KFZHaftpflicht")
           )
           ,tabPanel("Schaden am Eigentum"
                     , uiOutput("SchadenAmEigentum")
           )
           ,tabPanel("Rechtsstreit"
                     , uiOutput("Rechtsstreit")
           )
        )
        , addBugMuncher()
    )
  )
  
))

  
