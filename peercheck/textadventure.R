source("questions.R", encoding="UTF-8")

isApplicable <- function(answers, indexQ){
  requiredAnswers = ELQuestions[[indexQ]]$requiredAnswers
  
  result = T
  for (iterQ in names(requiredAnswers))  {
    result = result && is.element( answers[1,iterQ], unlist(requiredAnswers[iterQ], use.names=F) )
  }
  return(result)
}

n = length(ELQuestions)

# requires that answers already exists
# if not run : 
# answers = data.frame(rbind(rep(NA,n))) 
# colnames(answers) <- names(ELQuestions)  

  newanswers = data.frame(rbind(rep(NA,n)))
  colnames(newanswers) <- names(ELQuestions)  
  answers<-rbind(answers, newanswers)  
  print(answers)  


# if (is.null(answers)){
#   answers = data.frame(rbind(rep(NA,n)))
#   colnames(answers) <- names(ELQuestions)  
# }else{
# 
# newanswers = data.frame(rbind(rep(0,n)))
# colnames(newanswers) <- names(ELQuestions)
# 
# answers <- rbind(answers, newanswers)
# }

myRow = nrow(answers)


for (indexQ in sample(c(1:n))){
  if (isApplicable(answers[myRow,], indexQ)){
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
  }else{
    print(cat("\n Skipped: Question not applicable.\n\n"))
  }
}
print(answers)