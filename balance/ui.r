library(shiny)


shinyUI(bootstrapPage(
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Übersicht" 
      )
      
      ,tabPanel("Vermögen",
                showOutput("myChartAssets", "thetadrill")                       
      )
      ,tabPanel("Kredite",
                showOutput("myChartCredit", "thetadrill") 
      )      
      ,tabPanel("Einnahmen",
                showOutput("myChartIncome", "thetadrill")
      ) 
      ,tabPanel("Ausgaben",
                showOutput("myChartExpense", "thetadrill")
      )
    )
    
  )))
