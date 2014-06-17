getEmoticonHTML <<- function(happiness, size=50) {
  tags$embed(src=sprintf("../style/emoticons/emoticon-%02.0f0.svg", floor(happiness*10.9999)),
             style="position:relative",
             width=size, 
             height=size)
}

getSatisfactionHTML <<- function(questionid, value){
  res <- getSatisfactionPctPerQ(questionid)
  xml <- 
    tags$table(width="100%",
               lapply(names(res), function(name) {
                 score <- res[[name]]
                   tags$tr("data-ismyvalue" = if(!is.null(value)) name==value,
                     tags$td(width="30%", name),
                     if (is.na(score))
                       tags$td(tags$embed(src="../style/emoticons/sleeping.svg", width=50, height=50))
                     else
                       tags$td(
                         tags$div(style=sprintf('width:%f%%;background-color:gray;height:5px;float:left;margin-top:22px',score*0.8)),
                         getEmoticonHTML(score/100)
                       )
                   )
                 }
                 
               )
    )
  as.character(xml)
}