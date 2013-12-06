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
           tabPanel("Risikoabdeckung", chartOutput("myChart","datatables"))
           ,tabPanel("Vertragsoptimierung")
           ,tabPanel("Annahmen")
           )
    )
  )
  
))
