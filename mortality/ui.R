library(shiny)

# Define UI for application that plots random distributions 
shinyUI(bootstrapPage(
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Übersicht der Lebensphasen", showOutput("myChart", "nvd3"))
      ,tabPanel("Details", plotOutput("mortalityPlot"))
    )
    
  )
))