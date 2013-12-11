library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
      
  # Show a plot of the generated distribution
  mainPanel(
    div(style="width:800px"
        ,tags$h4("Du hast soeben den ersten Report freigeschaltet!")
        ,tags$p("Weitere Berichte erreichst Du unter Reports.")
        ,plotOutput("welcomeImg",  width = "100%")
    )
  )
  
))
