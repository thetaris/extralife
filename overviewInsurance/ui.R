library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
  
  # Application title
  headerPanel("Bericht: Versicherungsanalyse"),
  
  # Sidebar with a slider input for number of observations
    
    
     
  
  # Show a plot of the generated distribution
  mainPanel(
#    plotOutput("distPlot")
    div(style="width:800px", 
        tabsetPanel(
           tabPanel("??bersicht"
                    , uiOutput("overviewTable")                    
                    )
           ,tabPanel("private Haftpflicht"
                     , tags$h3("vorhandene Vertr??ge:")   
                     , tableOutput("haftpflicht1")
                     , uiOutput("haftpflicht2")
                     )
           ,tabPanel("Krankheit"
              )
           ,tabPanel("Invalidit??t"
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

  
