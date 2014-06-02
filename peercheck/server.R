library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  source('demodata.R')
  
  output$choose_dataname1 <- renderUI({
    selectInput("dataset1", "Data name X", as.list(names(data)))
  })
  
  output$choose_dataname2 <- renderUI({
    selectInput("dataset2", "Data name Y", c(as.list(names(data)), "NONE"))
  })
  
  output$distPlot <- renderPlot({
    choice1 =input$dataset1
    choice2 = input$dataset2
    # draw the histogram with the specified number of bins
    if(!is.null(choice1) && !is.null(choice2)){
      if(choice2 != "NONE")
        plot(jitter(data[, choice1]),jitter(data[, choice2]))
      else 
        hist(data[, choice1])
    }
  })
})