getEmoticonHTML <<- function(happiness, size=50) {
  tags$embed(src=sprintf("http://liquidizer.org/emoticons/face.svg?size=%d&a=%f&v=%f&p=1", size, happiness, happiness),
            style="position:relative",
             width=size, 
             height=size)
}

getSatisfactionHTML <<- function(questionid){
    res <- getSatisfactionPctPerQ(questionid)
    xml <- 
        tags$table(width="100%",
            lapply(names(res), function(name) {
              score <- res[[name]]
              tags$tr(
                  tags$td(width="30%", name),
                  if (is.na(score))
                    tags$td(tags$embed(src="http://liquidizer.org/emoticons/face.svg?size=50&view=sleeping", width=50, height=50))
                  else
                    tags$td(
                      tags$div(style=sprintf('width:%f%%;background-color:gray;height:5px;float:left;margin-top:22px',score*0.9)),
                      getEmoticonHTML(score/100)
                    )
              )
          })
        )
    as.character(xml)
}