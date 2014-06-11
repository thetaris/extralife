getSatisfactionHTML <<- function(questionid){
    res <- getSatisfactionPctPerQ(questionid)
    xml <- tags$div(lapply(c(1:length(res)), function(iterA) {  
      tags$div(
        sprintf("%30s hat Glücksscore %2.0f", names(res)[iterA], res[[iterA]]),
        if (!is.na(res[[iterA]])) {
          tags$div(style="width:100%",
            tags$div(style=sprintf('width:%d%%;background-color:green;height:10px',res[[iterA]],'%')
                      )
          )
        } else ""
      )
    })
    )
    as.character(xml)
}