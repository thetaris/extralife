library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Shiny App!"),
  
  # Sidebar with two select inputs
  sidebarLayout(
    sidebarPanel(
       uiOutput("choose_dataname1"),
       uiOutput("choose_dataname2"),
       uiOutput("enterselectvalue")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))