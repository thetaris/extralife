library(shiny)

# Define UI for application that plots random distributions 
#
shinyUI(bootstrapPage(
  # Show a plot of the generated distribution
  mainPanel(
    div(style="width:800px", tabsetPanel(
      tabPanel("Übersicht der Lebensphasen" 
               ,showOutput("life_phases", "nvd3")
               )
      
      ,tabPanel("Vergleich mit Deutschland"
                ,sliderInput("year", "Jahr", 
                             min=2009, max=2060, value=2013, animate=animationOptions(interval=4000, loop=T))
                ,showOutput("demography", "nvd3")
              )
      
      ,tabPanel("Details"   
               ,uiOutput("dataNames")
                ,showOutput("mortality", "nvd3")                  
              )

    ))
    
  )
))