library(shiny)

source("../common//utilEL.R")

shinyUI(bootstrapPage(
  
  # Show a plot of the generated distribution
  mainPanel(
    tags$div(style="width:800px",
    tabsetPanel(
      tabPanel("Übersicht",
               div(id="ovTop"),
               #htmlOutput("ovFlowHtml"),
               uiOutput("ovFlowHtml"),
               div(id="ovSep"),
               uiOutput("ovStatHtml")
            
      )
      ,tabPanel("Einnahmen",
                div(id="tableContIncome",
                    showOutput("myChartIncome", "thetadrill"),
                    uiOutput("myIncomeTable"),
                    textInput("myincomelevel", "", value="")
                ),
                div(id="alterContIncome",  
                    p(paste("Du hast noch kein Einkommen erfasst."))
                )              
                
      ) 
      ,tabPanel("Ausgaben",
                div(id="tableContExpense",
                    showOutput("myChartExpense", "thetadrill"),
                    uiOutput("myExpenseTable"),
                    textInput("myexpenselevel", "", value="")
                ),
                div(id="alterContExpense",  
                    p(paste("Du hast noch keine Ausgaben erfasst."))
                )                
                
      )      
      ,tabPanel("Vermögen",
                div(id="tableContAsset",
                  showOutput("myChartAssets", "thetadrill"),              
                  uiOutput("myAssetTable"),
                  textInput("myassetlevel", "", value="")
                ),
                div(id="alterContAsset",  
                    p(paste("Du hast noch keine Vermögenswerte erfasst."))
                )
                
      )
      ,tabPanel("Kredite",
                div(id="tableContCredit",
                  showOutput("myChartCredit", "thetadrill"), 
                    uiOutput("myCreditTable"),
                  textInput("mycreditlevel", "", value="")
                ),
                div(id="alterContCredit",  
                    p(paste("Du hast noch keine Kredite erfasst."))
                )
                
      )      
      
    )
    , addBugMuncher()
    )
  )))
