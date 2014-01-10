library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
  
  # Application title
  # headerPanel("Versicherungsanalyse") ,
  
  # Sidebar with a slider input for number of observations
    
    
     
  
  # Show a plot of the generated distribution
  mainPanel(
    tags$table(
      list(tags$td(tags$h2("Versicherungsanalyse "))
           ,tags$td("")      
           ,tags$td(selectInput("variable"," ", choices=list("Möglichst viel absichern" = "viel", 
                     "Ausgewogene Mischung" = "mittel", 
                     "Sparen, zahle kleine Schäden selbst" = "wenig"))
      ))
      ,width="100%"),
    
#    plotOutput("distPlot")
    div(style="width:800px", 
        tabsetPanel(
           tabPanel("Übersicht"
                    , uiOutput("overviewTable")                    
                    )
           ,tabPanel("private Haftpflicht"
                     , tags$h3("vorhandene Verträge:")   
                     , tableOutput("haftpflicht1")
                     , uiOutput("haftpflicht2")
                     )
           ,tabPanel("Krankheit"
              )
           ,tabPanel("Invalidität"
                     , tags$h3("vorhandene Verträge:")
                     , tags$p("staatliche Rente")
                     , tags$br()
                     , tags$h3(sprintf("Absicherung: %s", "minimal"))
                     , tags$p("Empfehlung: Informiere Dich über den Abschluß einer Beruftsunfähigkeitsversicherung (kurz BU) oder einer Unfallversicherung.")     
                     , tags$a("Details", href="http://cloud.thetaris.com/activateReport?report=1510&active=true")
           )
           ,tabPanel("Tod"
           )
           ,tabPanel("Schaden am Auto"
           )
           ,tabPanel("KFZ Haftpflicht"
           )
           ,tabPanel("Schaden am Eigentum"
           )
           ,tabPanel("Rechtsstreit"
           )
        )
    )
  )
  
))

  
