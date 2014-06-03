source("questions.R", encoding="UTF-8")

isApplicable <- function(answers, indexQ){
  requiredAnswers = ELQuestions[[indexQ]]$requiredAnswers
  
  result = T
  for (iterQ in names(requiredAnswers))  {
    result = result && is.element( answers[1,iterQ], unlist(requiredAnswers[iterQ], use.names=F) )
  }
  return(result)
}

nextQuestion<-function(answers, n=3){
  allQuestions <- names(ELQuestions)
  
  result = list()
  
  for(iterNQ in c(1:3)){
    # find element with highest prio in allQuestions
    highestPrio = 0    
    bestQ <- ""
    for (iterQ in allQuestions){
      if (ELQuestions[[iterQ]]$priority>highestPrio){
        if (is.na(answers[[iterQ]])){
          if (isApplicable(answers, iterQ)){
            highestPrio <- ELQuestions[[iterQ]]$priority
            bestQ <- iterQ
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

if ("answers" %in% ls()){
  newanswers = data.frame(rbind(rep(NA,n)))
  colnames(newanswers) <- names(ELQuestions)  
  answers<-rbind(answers, newanswers)    
}else
{
  answers = data.frame(rbind(rep(NA,n))) 
  colnames(answers) <- names(ELQuestions)
}

print(answers)  

myRow = nrow(answers)

indexQ <- "lebenszufrieden"

while(indexQ!="q"){    
    txt = sprintf("\nDIE GRAUE SEITE\n")
    txt = sprintf("%s---------------\n\n", txt)
    txt = sprintf("%sFrage: %s\n\n", txt, ELQuestions[[indexQ]]$Text)    
    
    ansEnum = ELATYPE[[ELQuestions[[indexQ]]$AType]]$value
    for (iterAns in c(1:length(ansEnum))){
      txt = sprintf("%s%i) %s\n", txt, iterAns, ansEnum[[iterAns]])  
    }
    txt = sprintf("%s\n", txt)
    
    print(cat(txt))
    
    ans = readline("Ihre Antwort: ")
    answers[myRow, names(ELQuestions[indexQ])]<-ansEnum[[eval(parse(text=ans))]]
    
    print(cat(sprintf("\nAuswertung:\n\n")))
    
    txt<-sprintf("\nNächste Frage:\n")
    
    qEnum = nextQuestion(answers[myRow,])
    if (length(qEnum)>0){
      for (iterq in c(1:length(qEnum))){
        txt = sprintf("%s%i) %s\n", txt, iterq, ELQuestions[[qEnum[[iterq]]]]$shortText)  
      }
      txt = sprintf("%s\n", txt)
      print(cat(txt))
      
      ans = readline("gewünschte Frage (q zum Abbruch): ")
      indexQ<-tryCatch(qEnum[[eval(parse(text=ans))]], error=function(e) return("q"))      
    }else
    {
      # stop: no further questions available
      indexQ <- "q"
    }
}


print(answers)