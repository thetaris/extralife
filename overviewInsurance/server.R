library(shiny)
library(rCharts)
source("../common/INIT.R", chdir=T, encoding="UTF-8")

source("computations.R", encoding="UTF-8")
source("renderDetail.R", encoding="UTF-8")
#eval(parse("computations.R", encoding="UTF-8"))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  
  dataObj = isolate(DGSData(session=session))    
  
  versicherungen <- getVersicherungen(dataObj)
  besitz <- getBesitz(dataObj)
  familie <- getFamilie(dataObj)
  
  for (iterContracts in names(versicherungen)){
    #does not work in shiny:
    #output[iterContracts] <- renderDetail(versicherungen[iterContracts])    

    # -> build string and eval command
    cmdStr <- sprintf("output$%s <- renderDetail(versicherungen, besitz, familie, input, '%s')",  iterContracts, iterContracts)
    eval(parse(text=cmdStr))    
  }
    
  output$overviewTable <- renderOverview(versicherungen, besitz, familie, input)
  
  
})