library(shiny)
source("../common/utilEL.R")
# Define UI
#
shinyUI(bootstrapPage(
  
  mainPanel(
    div(style="width:800px", tabsetPanel(
      tabPanel("Übersicht der Lebensphasen" 
               ,showOutput("life_phases", "nvd3")
               )
      
      ,tabPanel("Vergleich mit Deutschland"
                ,sliderInput("year", "Wähle ein Jahr", 
                             min=2009, max=2060, value=2013, animate=animationOptions(interval=4000, loop=T))
                ,showOutput("demography", "nvd3")
              )
      
      ,tabPanel("Details"   
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