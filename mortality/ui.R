library(shiny)

# Define UI
#
shinyUI(bootstrapPage(
  
  mainPanel(
    div(style="width:800px", tabsetPanel(
      tabPanel("Übersicht der Lebensphasen"
               ,tags$h3("Wer ist wann in Ausbildung, Arbeit oder Ruhestand?")
               ,showOutput("life_phases", "nvd3")
               )
      
      ,tabPanel("Vergleich mit Deutschland"
                ,tags$h3("In welchen Altersgruppen sind wir?")
                ,sliderInput("year", "Wähle ein Jahr", 
                             min=2009, max=2060, value=as.numeric(format.Date(Sys.Date(), "%Y")), animate=animationOptions(interval=4000, loop=T))
                ,showOutput("demography", "nvd3")
              )
      
      ,tabPanel("Details"   
                ,tags$h3("Wie alt werden wir?")
               ,uiOutput("dataNames")
                ,showOutput("mortality", "nvd3")                  
              )
      ,tabPanel("Verwendete Daten"
                ,dataTableOutput("dataUsed")
              )
    )
    , addBugMuncher()
    )
    
  )
))