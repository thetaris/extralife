library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
      
  # Show a plot of the generated distribution
  mainPanel(
    div(style="width:760px"
        ,tags$h4("Du hast soeben Deine erste App freigeschaltet!")
        ,tags$p("Weitere Apps findest Du hier:")
        ,plotOutput("welcomeImg",  width = "100%")
    )
  )
  
))
