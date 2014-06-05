library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  source('demodata.R')
  
  output$choose_dataname1 <- renderUI({
    selectInput("dataset1", "Data name X", as.list(names(data)))
  })
  
  output$choose_dataname2 <- renderUI({
    selectInput("dataset2", "Data name Y", c(as.list(names(data)), "NONE"), selected = "NONE")
  })
  
  output$distPlot <- renderPlot({
    choice1 =input$dataset1
    choice2 = input$dataset2
    # draw the histogram with the specified number of bins
    if(!is.null(choice1) && !is.null(choice2)){
      if(choice2 != "NONE"){
        output$enterselectvalue <- renderUI({
          textInput("text2", label = choice2)
        })
        range1 = input$text1
        print(range1)
        range2 = input$text2
        print(range2)
        
        plot(jitter(data[, choice1]),jitter(data[, choice2]), xlab=choice1, ylab=choice2,
             col = ({
               logvec = data[, choice1] == rep(range1,1000) & data[, choice2] == rep(range2,1000)
               sapply(logvec, function(v)
                 if(v)
                   "red"
                 else
                   "black" 
               )                  
             })
        )
      }
      else {
        output$enterselectvalue <- renderUI({
          textInput("text1", label = choice1)
        })
        range = input$text1
        print(range)
        hist(data[, choice1], xlab = choice1, col = ({
#           sapply(data[, choice1] == rep(range,1000), function(v)
#             if(v)
#               "skyblue"
#             else
#               "white"
#           )
        })
        )
      }
    }
  })
})
