library(shiny)


shinyUI(bootstrapPage(
  
  # Show a plot of the generated distribution
  mainPanel(
    tags$div(style="width:1000px",
    tabsetPanel(
      tabPanel("Übersicht",
               div(id="ovTop"),
               htmlOutput("ovFlowHtml"),
               div(id="ovSep"),
               htmlOutput("ovStatHtml")
            
      )
      ,tabPanel("Einnahmen",
                div(id="tableContIncome",
                    showOutput("myChartIncome", "thetadrill"),
                    tableOutput("myIncomeTable"),
                    textInput("myincomelevel", "", value="")
                ),
                div(id="alterContIncome",  
                    p(paste("Du hast noch kein Einkommen erfasst :-( Wie soll ich da eine Graphik erstellen."))
                )              
                
      ) 
      ,tabPanel("Ausgaben",
                div(id="tableContExpense",
                    showOutput("myChartExpense", "thetadrill"),
                    tableOutput("myExpenseTable"),
                    textInput("myexpenselevel", "", value="")
                ),
                div(id="alterContExpense",  
                    p(paste("Du hast noch keine Ausgaben erfasst :-( Wie soll ich da eine Graphik erstellen."))
                )                
                
      )      
      ,tabPanel("Vermögen",
                div(id="tableContAsset",
                  showOutput("myChartAssets", "thetadrill"),              
                  tableOutput("myAssetTable"),
                  textInput("myassetlevel", "", value="")
                ),
                div(id="alterContAsset",  
                    p(paste("Du hast noch keine Vermögenswerte erfasst :-( Wie soll ich da eine Graphik erstellen."))
                )
                
      )
      ,tabPanel("Kredite",
                div(id="tableContCredit",
                  showOutput("myChartCredit", "thetadrill"), 
                  tableOutput("myCreditTable"),
                  textInput("mycreditlevel", "", value="")
                ),
                div(id="alterContCredit",  
                    p(paste("Du hast noch keine Kredite erfasst :-( Wie soll ich da eine Graphik erstellen."))
                )
                
      )      

    )
    )
  )))
