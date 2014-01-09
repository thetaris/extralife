library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
  
  # Application title
  headerPanel("Versicherungsanalyse") ,
  
  # Sidebar with a slider input for number of observations
    
    
     
  
  # Show a plot of the generated distribution
  mainPanel(
    selectInput("variable","Ich möchte", choices=list("möglichst viel absichern." = "viel", 
                     "eine ausgewogene Mischung aus selbst zahlen und versichern." = "mittel", 
                     "sparen und zahle kleine Schäden selbst." = "wenig")),
    
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

  
