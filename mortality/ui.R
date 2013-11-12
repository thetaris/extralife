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
<<<<<<< HEAD
                ,showOutput("demographyPlot", "nvd3")
      )
=======
               ,showOutput("demographyPlot", "nvd3")
              )
      
      ,tabPanel("Details"   
               ,uiOutput("dataNames")
#               ,plotOutput("mortalityPlot")
               ,showOutput("mortalityPlotRCharts", "nvd3")  
              )
>>>>>>> 5799601d895c3844e1aa129f15f4708d8baf288c
    )
    
  )
))