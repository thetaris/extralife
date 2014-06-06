isApplicable <<- function(answers, indexQ){
  requiredAnswers = ELQuestions[[indexQ]]$requiredAnswers
  
  result = T
  for (iterQ in names(requiredAnswers))  {
    result = result && is.element( answers[1,iterQ], unlist(requiredAnswers[iterQ], use.names=F) )
  }
  return(result)
}

getSatisfaction <<- function(answers){
  lebenszufrieden<-list()
  lebenszufrieden[["D1"]]<-answers["lebenszufriedenD1"]
  lebenszufrieden[["D2"]]<-answers["lebenszufriedenD2"]
  lebenszufrieden[["D3"]]<-answers["lebenszufriedenD3"]
  lebenszufrieden[["D4"]]<-answers["lebenszufriedenD4"]
  lebenszufrieden[["D5"]]<-answers["lebenszufriedenD5"]
  
  score <- 0
  
  for (iterID in lebenszufrieden){
    score <- score + which(ELATYPE$stimmt$value==iterID[[1]])
  }
  return(score)  
}

getSatisfactionText<<-function(score){
  if (is.null(score) 
      || length(score)==0
      || is.na(score)) return(NULL)
  if (score<10) return("extrem unzufrieden")
  if (score<15) return("unzufrieden")
  if (score<20) return("leicht unterdurchschnittlich zufrieden")
  if (score<25) return("durchschnittlich zufrieden")
  if (score<30) return("überdurchschnittlich zufrieden")
  return("überaus zufrieden")
}

getSatisfactionPct<<-function(score){
  return( round( (score-5)/30 * 100 )  )
}

getSatisfactionPctPerQ<<-function(indexQ){
  res<-list()
  for (iterField in ELATYPE[[ELQuestions[[indexQ]]$AType]]$value){        
    tmpAns <- answers[which( answers[,indexQ]==iterField[[1]] ),]
    tmp <- unlist( sapply(c(1:myRow), function(index) return(getSatisfaction(tmpAns))) )    
    if (length(tmp)>0){
      print(is.null(tmp))
      res[[iterField[[1]]]] <- median(tmp)
    }else{
      res[[iterField[[1]]]] <- NA
    }
      
  }
  return(res)
}

saveQuestion <<- function(userid, questionid, value) {
  if (!any(answers$userid == userid)) {
    answers[nrow(answers)+1,'userid'] <<- userid
  }
  answers[answers$userid==userid,questionid] <<- value
}

nextQuestion <<- function(userid, n=3){
  allQuestions <- names(ELQuestions)
  
  if (!any(answers$userid == userid)) {
    answers[nrow(answers)+1,'userid']=userid
  }
  row <- answers[answers$userid == userid,]
  
  result = list()
  
  for(iterNQ in c(1:3)){
    # find element with highest prio in allQuestions
    highestPrio = 0    
    bestQ <- ""
    for (iterQ in allQuestions){
      if (ELQuestions[[iterQ]]$priority>highestPrio){
        if (is.na(row[[iterQ]])){
          if (isApplicable(row, iterQ)){
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


n = length(ELQuestions)

# requires that answers already exists
# if not run : 

# SWITCH TO EXPANDED DATA STRUCTURE
# if ("answers" %in% ls()){
#   newanswers = data.frame(rbind(rep(NA,n+1)))
#   colnames(newanswers) <- c('userid', names(ELQuestions))
#   
#   # add cols for new questions
#   answers<-cbind(answers, newanswers[,!(names(newanswers) %in% names(answers))])
#   colnames(answers) <- names(ELQuestions)  
#   
#   answers<-rbind(answers, newanswers)    
# }else
answers <<- data.frame(rbind(rep(NA, n+1))) 
colnames(answers) <- c('userid', names(ELQuestions))
answers[1,'userid'] = 'testuser'

# TEXT ADVENTURE
# myRow = nrow(answers)
# indexQ <- "lebenszufriedenD1"
# 
# while(indexQ!="q"){    
#   txt = sprintf("\nDIE GRAUE SEITE\n")
#   txt = sprintf("%s---------------\n\n", txt)
#   txt = sprintf("%sFrage: %s\n\n", txt, ELQuestions[[indexQ]]$Text)    
#   
#   ansEnum = ELATYPE[[ELQuestions[[indexQ]]$AType]]$value
#   for (iterAns in c(1:length(ansEnum))){
#     txt = sprintf("%s%i) %s\n", txt, iterAns, ansEnum[[iterAns]])  
#   }
#   txt = sprintf("%s\n", txt)
#   
#   print(cat(txt))
#   
#   ans = readline("Ihre Antwort: ")
#   answers[myRow, names(ELQuestions[indexQ])]<-ansEnum[[eval(parse(text=ans))]]
#   
#   #### Results
#   # if is result of first part
#   if (ELQuestions[[indexQ]]$priority==9999){
#     score<-getSatisfaction(answers[myRow,])
#     #if all questions answered
#     if (length(score)>0){
#       print(cat(sprintf("\nAuswertung:\nIhr Zufriedenheitsindex: %i von 100 (%s)\n", getSatisfactionPct(score), getSatisfactionText(score))))
#       allScores<-sapply(c(1:myRow), function(index) return(getSatisfaction(answers[index,])))
#       allScoresTxt<-sapply(c(1:myRow), function(index) return(getSatisfactionText(getSatisfaction(answers[index,]))))
#       hist(unlist(allScores))
#       txt = "Übersicht über die Antworten:\n"
#       for (iterS in 1:6){       
#         sTxt<-getSatisfactionText(iterS*5)
#         txt <- sprintf("%s%40s : %5i\n", txt, sTxt, sum(allScoresTxt==sTxt))
#       }
#       print(cat(txt,'\n',''))
#     }
#   }else{
#     txt = "Übersicht über die Antworten:\n"
#     
#     res<-getSatisfactionPctPerQ(indexQ)
#     for (iterA in c(1:length(res))){
#       txt <- sprintf("%s\n%30s hat Glücksscore %i", txt, names(res)[iterA], res[[iterA]])
#     }
#     print(cat(txt))
#   }
#   
#   txt<-sprintf("\nNächste Frage:\n")
#   
#   qEnum = nextQuestion(answers[myRow,])
#   if (length(qEnum)==1){
#     indexQ <- qEnum[[1]]
#   }
#   else {
#     if (qEnum[[1]]!=""){
#       for (iterq in c(1:length(qEnum))){
#         if (qEnum[[iterq]]!=""){
#           txt = sprintf("%s%i) %s\n", txt, iterq, ELQuestions[[qEnum[[iterq]]]]$shortText)            
#         }
#       }
#       txt = sprintf("%s\n", txt)
#       print(cat(txt))
#       
#       ans = readline("gewünschte Frage (q zum Abbruch): ")
#       indexQ<-tryCatch(qEnum[[eval(parse(text=ans))]], error=function(e) return("q"))      
#     }else
#     {
#       # stop: no further questions available
#       indexQ <- "q"
#     }
#   }
# }
# 
# 
# print(answers)