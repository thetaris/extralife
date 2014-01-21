library(shiny)
library(rCharts)

# Define UI for application that plots random distributions 
shinyUI(basicPage(
  
  # Application title
  # headerPanel("Versicherungsanalyse") ,
  
  # Sidebar with a slider input for number of observations
    
    
     
  
  # Show a plot of the generated distribution
  mainPanel(
    tags$table(
      list(tags$td(tags$h2("Versicherungsanalyse "))
           ,tags$td("")      
           ,tags$td(selectInput("variable"," ", choices=list("Möglichst viel absichern" = "viel", 
                     "Ausgewogene Mischung" = "mittel", 
                     "Sparen, zahle kleine Schäden selbst" = "wenig"))
      ))
      ,width="100%"),
    
#    plotOutput("distPlot")
    div(style="width:800px", 
        tabsetPanel(
           tabPanel("Übersicht"
                    , uiOutput("overviewTable")                    
                    )
           ,tabPanel("private Haftpflicht"
                     , uiOutput("haftpflicht")
                     )
           ,tabPanel("Krankheit"
                     , uiOutput("Krankheit")
              )
           ,tabPanel("Invalidität"
                     , uiOutput("Invaliditaet")
           )
           ,tabPanel("Tod"
                     , uiOutput("Tod")
           )
           ,tabPanel("Schaden am Auto"
                     , uiOutput("SchadenAmAuto")
           )
           ,tabPanel("KFZ Haftpflicht"
                     , uiOutput("KFZHaftpflicht")
           )
           ,tabPanel("Schaden am Eigentum"
                     , uiOutput("SchadenAmEigentum")
           )
           ,tabPanel("Rechtsstreit"
                     , uiOutput("Rechtsstreit")
           )
        )
        , tags$script('var bugmuncher_options = {
		language: "en",
		position: "right",
		require_email: false,
		api_key: "c413a62781c1473f2943739d94e472b46dd84036"
	};
	(function(){ 
		var node = document.createElement("script"); 
		node.setAttribute("type", "text/javascript"); 
		node.setAttribute("src", "//app.bugmuncher.com/js/bugMuncher.min.js"); 
		document.getElementsByTagName("head")[0].appendChild(node); 
	})();
', type="text/javascript")
    )
  )
  
))

  
