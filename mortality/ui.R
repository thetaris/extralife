library(shiny)

# Define UI for application that plots random distributions 
shinyUI(bootstrapPage(
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("mortalityPlot")
  )
))