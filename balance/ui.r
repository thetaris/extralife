library(shiny)


shinyUI(bootstrapPage(
  
  # Show a plot of the generated distribution
  mainPanel(
    tags$div(style="width:1000px",
    tabsetPanel(
      tabPanel("Übersicht" 
      )
      
      ,tabPanel("Vermögen",
                showOutput("myChartAssets", "thetadrill"),
                tableOutput("myAssetTable"),
                textInput("myassetlevel", "", value="")
      )
      ,tabPanel("Kredite",
                showOutput("myChartCredit", "thetadrill"), 
                tableOutput("myCreditTable"),
                textInput("mycreditlevel", "", value="")
      )      
      ,tabPanel("Einnahmen",
                showOutput("myChartIncome", "thetadrill"),
                tableOutput("myIncomeTable"),
                textInput("myincomelevel", "", value="")
      ) 
      ,tabPanel("Ausgaben",
                showOutput("myChartExpense", "thetadrill"),
                tableOutput("myExpenseTable"),
                textInput("myexpenselevel", "", value="")
      )
    )
    )
  )))
