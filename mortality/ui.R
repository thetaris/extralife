library(shiny)

# Define UI for application that plots random distributions 
shinyUI(bootstrapPage(
  
  # load data
  
  
  #load data
  
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Übersicht der Lebensphasen" 
               ,showOutput("myChart", "nvd3")
               )
      
      ,tabPanel("Details"   
               ,uiOutput("dataNames")
               ,showOutput("mortalityPlotRCharts", "nvd3")  
               ,plotOutput("mortalityPlot")
              )
      ,tabPanel("Vergleich mit Deutschland"
                ,sliderInput("year", "Jahr", 
                             min=2009, max=2060, value=2013, animate=animationOptions(interval=4000, loop=T))
                ,showOutput("demographyPlot", "nvd3")
      )
    )
    
  )
))