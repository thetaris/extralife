library(shiny)
library(rCharts)

source("../common/utilEL.R")

# Define UI for application that plots random distributions 
shinyUI(basicPage(
  
  # Application title
  headerPanel("Risiko: Invalidität"),
  
#   # Sidebar with a slider input for number of observations
#  sidebarPanel(
#     
#     tabsetPanel(
#       tabPanel("Szenario",  
#         
#       radioButtons("cause", "Was passiert?",
#                    list("Ich kann weiter voll arbeiten." = "healthy",
#                         "Ich werde krank." = "rnorm",
#                         "Ich habe einen Arbeitsunfall." = "unif",
#                         "Ich habe einen Unfall in der Freizeit." = "lnorm")),
#     
#     conditionalPanel(
#       condition = "input.cause != 'healthy'",
#       
#     br(),
#       
#       radioButtons("duration", "Wie geht es nach 2 Jahren weiter?",
#                    list("Ich kann wieder voll arbeiten." = "full",
#                         "Ich kann in einem anderen Beruf voll arbeiten." = "fullOther",
#                         "Ich kann in einem anderen Beruf weniger als 6h pro Tag arbeiten." = "sixHours",
#                         "Ich kann überhaupt nicht mehr arbeiten." = "notWorking")),
#       
#       br()
#       
#     ), 
#     sliderInput("bu_annuity", 
#                   "Beitrag zur Berufsunfähighkeitsversicherung (in EUR)", 
#                   min = 0,
#                   max = 500, 
#                   value = 0)
#       ),
#       tabPanel("Annahmen")  
#      )
#  ),
  
  # Show a plot of the generated distribution
  div(style="width:770px", mainPanel(
     tabsetPanel(
       tabPanel("Übersicht"
                , tags$h3("Dein Arbeitslohn bis zur Rente")
                , chartOutput("overview","nvd3"))
       ,tabPanel("Risiko"
                 ,div(style="width:770px"
                     , tags$h3("Deine Wahrscheinlichkeit berufsunfähig zu werden nach Alter")
                     , chartOutput("risk","nvd3"))
                 )
       )
  , addBugMuncher()
     )
  )
))
