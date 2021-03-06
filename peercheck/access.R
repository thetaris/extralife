isApplicable <<- function(userid, indexQ){
  row<-getAnswerRow(userid)
  
  requiredAnswers = ELQuestions[[indexQ]]$requiredAnswers
  
  result = T
  for (iterQ in names(requiredAnswers))  {
    result = result && is.element( row[1,iterQ], unlist(requiredAnswers[iterQ], use.names=F) )
  }
  return(result)
}

isValidUser <<- function(userid) {
   !is.null(userid) && nchar(userid) > 0 && any(ELpeercheck$userid == userid)
}

getSatisfaction <<- function(userid){
  row<-getAnswerRow(userid)
  getSatisfactionFromRow(row)
}

getSatisfactionFromRow <<- function(row){
  lebenszufrieden<-list()
  lebenszufrieden[["D1"]]<-row["lebenszufriedenD1"]
  lebenszufrieden[["D2"]]<-row["lebenszufriedenD2"]
  lebenszufrieden[["D3"]]<-row["lebenszufriedenD3"]
  lebenszufrieden[["D4"]]<-row["lebenszufriedenD4"]
  lebenszufrieden[["D5"]]<-row["lebenszufriedenD5"]
  
  score <- 0
  
  for (iterID in lebenszufrieden){
    score <- score + which(ELATYPE$stimmt$value==iterID[[1]])
  }
  return(score)  
}

getSatisfactionText<<-function(score){
  ans<-list("extrem unzufrieden", "unzufrieden", "leicht unterdurchschnittlich zufrieden", "durchschnittlich zufrieden", "überdurchschnittlich zufrieden", "überaus zufrieden")
  ans<-transformIfWindows(ans)
  if (is.null(score) 
      || length(score)==0
      || is.na(score)) return(NULL)
  if (score<10) return(ans[1])
  if (score<15) return(ans[2])
  if (score<20) return(ans[3])
  if (score<25) return(ans[4])
  if (score<30) return(ans[5])
  return(ans[6])
}

getSatisfactionPct<<-function(score){
  return( round( (score-5)/30 * 100 )  )
}


getHeader <<- function(){
  theheader <- readLines("header.html")
  return (paste(theheader, collapse="\n"))
  
}

getFooter <<- function(){
  thefooter <- readLines("footer.html")
  return (paste(thefooter, collapse="\n"))
}

getSatisfactionPctPerQ <<- function(indexQ){
  res<-list()
  answers <- getAnswerTable()
  for (iterField in ELATYPE[[ELQuestions[[indexQ]]$AType]]$value){        
    tmpAns <- answers[which( answers[,indexQ]==iterField[[1]] ),]
    tmp <- unlist( sapply(c(1:nrow(tmpAns)), function(index) return(getSatisfactionPct(getSatisfactionFromRow(tmpAns[index,])))) )    
    if (length(tmp)>0){      
      res[[iterField[[1]]]] <- median(tmp)
    }else{
      res[[iterField[[1]]]] <- NA
    }
    
  }
  return(res)
}

# load ELAnswers form disk if possible
# 
if(file.exists("log.RData")){
  #ELpeercheck <<- read.csv("ans.csv", sep=",")
  # load the log.RData file and all the data are directly loaded to variable ELpeercheck
  load("log.RData")
}else{
  ELpeercheck <<- c()
}

saveQuestion <<- function(userid, questionid, value) {
  # append a row to ELpeercheck with values for: userid, Timestamp, questionid, value
  if (!is.null(ELpeercheck$Timestamp)) {
    ELpeercheck = cbind(data.frame(time=strptime(ELpeercheck$Timestamp, format="%Y_%m_%d_%H:%M:%S")),
                        ELpeercheck[,c('userid','questionid','value')])
  }
  ans <- data.frame(userid=userid, time=Sys.time(), questionid=questionid, value=value)
  ELpeercheck <<- rbind(ELpeercheck, ans)
  # save ELpeercheck to disk
  save(ELpeercheck, file ="../log.RData")
}

getAnswerRow<<-function(userid_in){
  values<-as.character( ELpeercheck[ELpeercheck[,"userid"]==userid_in,"value"] ) 
  questionids<-as.character(ELpeercheck[ELpeercheck[,"userid"]==userid_in,"questionid"])
  
  allQuestions<-names(ELQuestions)  
  
  newAnswers <- data.frame(rbind(rep(NA, length(allQuestions)+1)), stringsAsFactors=F)
  colnames(newAnswers) <- c("userid", allQuestions)
  
  for (index in c(1:length(values)))
    newAnswers[1, questionids[[index]] ] <- values[[index]] 
  
  return(newAnswers)
}

getAnswerTable <<- function(){  
  questions <- names(ELQuestions) 
  newAnswers <- Reduce(rbind,
    lapply(unique(ELpeercheck[,"userid"]), function(user) {
      getAnswerRow(user)[questions]
  }), c())
  return( data.frame(newAnswers))
}

getNumUsers<<-function(){
  length(unique(ELpeercheck[,"userid"]))
}

getUsers<<-function(){
  (unique(ELpeercheck[,"userid"]))
}

nextQuestion <<- function(userid, n=3){
  allQuestions <- names(ELQuestions)
  
  row <- getAnswerRow(userid)
  
  result = list()
  
  for(iterNQ in c(1:3)){
    # find element with highest prio in allQuestions
    highestPrio = 0    
    bestQ <- ""
    for (iterQ in allQuestions){
      if (ELQuestions[[iterQ]]$priority>highestPrio){
        if (is.na(row[[iterQ]])){
          if (isApplicable(userid, iterQ)){
            
            highestPrio <- as.numeric(ELQuestions[[iterQ]]$priority)
            bestQ <- iterQ
            #             if (highestPrio==9999){
            #               result <- append(result, bestQ)
            #               return(result)
            #             }
          }
        }
      }
    }
    # remove element with highest prio from list of allQuestions
    allQuestions <- allQuestions[allQuestions != bestQ]
    # remember best element in question list
    result <- append(result, bestQ)    
  }
  return(result)
}
