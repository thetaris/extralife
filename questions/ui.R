library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
      
  # Show a plot of the generated distribution
  mainPanel(
    div(style="width:800px"
        ,tags$iframe(src="https://docs.google.com/forms/d/1aEGhYU961Mh2yIXux5LkHBxdbKY5sNUsuRzLFi91P88/viewform?embedded=true"
                     , width="760"
                     , height="760"
                     , frameborder="0"
                     , marginheight="0"
                     , marginwidth="0")        
    )
  )
  
))
