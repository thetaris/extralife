isApplicable <<- function(userid, indexQ){
  row<-getAnswerRow(userid)
  
  requiredAnswers = ELQuestions[[indexQ]]$requiredAnswers
  
  result = T
  for (iterQ in names(requiredAnswers))  {
    result = result && is.element( row[1,iterQ], unlist(requiredAnswers[iterQ], use.names=F) )
  }
  return(result)
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

getSatisfactionPctPerQ<<-function(indexQ){
  res<-list()
  answers<-getAnswerTable()
  for (iterField in ELATYPE[[ELQuestions[[indexQ]]$AType]]$value){        
    tmpAns <- answers[which( answers[,indexQ]==iterField[[1]] ),]
    tmp <- unlist( sapply(c(1:nrow(answers)), function(index) return(getSatisfactionPct(getSatisfactionFromRow(tmpAns)))) )    
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
  now <- Sys.time()
  ans <- data.frame(userid=userid, Timestamp = format(now, "%Y_%m_%d_%H:%M:%S") ,  questionid=questionid, value=value)
  ELpeercheck <<- rbind(ELpeercheck, ans)
  # save ELpeercheck to disk
  save(ELpeercheck, file ="../log.RData")
  #write.csv(ELpeercheck, file ="../log.csv",row.names=FALSE)
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

getAnswerTable<<-function(){  
  firstUser<- getUsers()[1]
  
  newAnswers <- NULL
  
  for(user in unique(ELpeercheck[,"userid"]) ){newAnswers<-rbind(newAnswers, getAnswerRow(user)) }
  
  return( newAnswers)
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
            highestPrio <- ELQuestions[[iterQ]]$priority
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
