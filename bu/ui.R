library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
  
  # Application title
  # headerPanel("Versicherungsanalyse") ,
  
  # Sidebar with a slider input for number of observations
  
  
  
  
  # Show a plot of the generated distribution
  elTags$datasafe.float(text="Daten ändern", task="1717"),
  mainPanel(
    tags$table(
      list(tags$td(tags$h2("Risiko Invalidität"))
           ,tags$td("Wer?")      
           ,tags$td(selectInput("Person"," ", choices=list("ich" = "ich"
                                                        ))
           ))
      ,width="100%"),
    
    #    plotOutput("distPlot")
    div(style="width:790px", 
        tabsetPanel(
          tabPanel("Übersicht"
                   , tags$h4("Dein Arbeitslohn bis zur Rente")
                   , chartOutput("overview","nvd3")
          )
          ,tabPanel("Risiko"
                    ,div(style="width:770px; overflow:visible;"
                         , tags$h4("Wie viele Personen in Deiner Situation werden in welchem Alter berufsunfähig sein?")
                         , chartOutput("risk","nvd3"))
                         , tags$div(align="center", tags$h4("Wahrscheinlichkeit basierend auf Alter, Geschlecht und Tätigkeit"))
                         , tags$div(align="center", uiOutput("dataProf"))
          )
        )
        , addBugMuncher()
    )
  )
  
))


# 
# 
# library(shiny)
# library(rCharts)
# 
# source("../common/utilEL.R")
# 
# # Define UI for application that plots random distributions 
# shinyUI(basicPage(
#   
#   # Application title
#   
# #   # Sidebar with a slider input for number of observations
# #  sidebarPanel(
# #     
# #     tabsetPanel(
# #       tabPanel("Szenario",  
# #         
# #       radioButtons("cause", "Was passiert?",
# #                    list("Ich kann weiter voll arbeiten." = "healthy",
# #                         "Ich werde krank." = "rnorm",
# #                         "Ich habe einen Arbeitsunfall." = "unif",
# #                         "Ich habe einen Unfall in der Freizeit." = "lnorm")),
# #     
# #     conditionalPanel(
# #       condition = "input.cause != 'healthy'",
# #       
# #     br(),
# #       
# #       radioButtons("duration", "Wie geht es nach 2 Jahren weiter?",
# #                    list("Ich kann wieder voll arbeiten." = "full",
# #                         "Ich kann in einem anderen Beruf voll arbeiten." = "fullOther",
# #                         "Ich kann in einem anderen Beruf weniger als 6h pro Tag arbeiten." = "sixHours",
# #                         "Ich kann überhaupt nicht mehr arbeiten." = "notWorking")),
# #       
# #       br()
# #       
# #     ), 
# #     sliderInput("bu_annuity", 
# #                   "Beitrag zur Berufsunfähighkeitsversicherung (in EUR)", 
# #                   min = 0,
# #                   max = 500, 
# #                   value = 0)
# #       ),
# #       tabPanel("Annahmen")  
# #      )
# #  ),
#   
#   # Show a plot of the generated distribution
#   div(style="width:770px; overflow:visible;"
#       , mainPanel(tags$h1("Risiko Invalidität")
#      , tabsetPanel(
#        tabPanel("Übersicht"
#                 , tags$h3("Dein Arbeitslohn bis zur Rente")
#                 , chartOutput("overview","nvd3"))
#        ,tabPanel("Risiko"
#                  ,div(style="width:770px; overflow:visible;"
#                      , tags$h3("Deine Wahrscheinlichkeit berufsunfähig zu werden nach Alter")
#                      , chartOutput("risk","nvd3"))
#                  )
#        )
#   , addBugMuncher()
#      )
#   )
# ))
