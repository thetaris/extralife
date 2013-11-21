library(shiny)


shinyUI(bootstrapPage(

  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Übersicht" 
               )
      
      
      ,tabPanel("Vermögensstatus",
                showOutput("myChartAssets", "thetadrill"),
                showOutput("myChartCredit", "thetadrill")
          
                )
      
      
      ,tabPanel("Einnahmen/Ausgaben",
                showOutput("myChartIncome", "thetadrill"),
                showOutput("myChartExpense", "thetadrill")
                )
      )
      
    )))
