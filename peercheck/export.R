library(rjson)
source('questions.R', encoding='UTF-8')

xstr <- toJSON(list(questions=ELQuestions, atypes=ELATYPE))
writeChar(xstr, file('questions.json'))