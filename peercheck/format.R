getSatisfactionHTML <<- function(questionid){
  res<-getSatisfactionPctPerQ(questionid)
  sapply(c(1:length(res)), function(iterA) {  
    sprintf("<br/>%30s hat Glücksscore %2.0f<br/>", names(res)[iterA], res[[iterA]])
  })
}