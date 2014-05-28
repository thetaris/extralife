getELACAT <- function(){
  allELACAT <- function(){
    return(c(    
      "Krankenversicherung", 
      "Autoversicherung"))  
  }
  ELACAT <<- list()
  ELACAT[allELACAT()]<<-allELACAT()
}
getELACAT()

getAType <- function() {
  addToList <- function(field, type, range){    
    ELATYPE[[field]] <<- list()
    ELATYPE[[field]]$type <<- type
    ELATYPE[[field]]$value <<- range
    ELATYPE[[field]]$key <<- field
    
    if (type == "enum"){
      for (iterRange in range){
        ELA[[iterRange]] <<- iterRange
      }
    }
  }  
  
  ELA <<- list()
  ELATYPE <<- list()
  
  addToList("jaNein",     "enum", list("Ja", "Nein")) 
  addToList("wichtig",    "enum", list("sehr wichtig", "wichtig", "unwichtig", "sehr unwichtig"))
  addToList("angemessen", "enum", list("zu hoch", "angemessen", "zu niedrig"))
  addToList("gesetzlichPrivat", "enum", list("gesetzlich", "privat", "gar nicht"))
  addToList("zufrieden",  "enum", list("sehr unzufrieden", "unzufrieden", "zufrieden", "sehr zufrieden"))
}

getAType()



getQuestions <- function() {
  addToList <- function(ID, Text, ATypeKey, category, requiredAnswers = NULL){
    ELQuestions[[ID]] <<- list()
    ELQuestions[[ID]]$Text <<- Text
    ELQuestions[[ID]]$AType <<- ATypeKey
    ELQuestions[[ID]]$category <<- category
    ELQuestions[[ID]]$requiredAnswers <<- requiredAnswers
  }
  
  ELQuestions <<- list()
  addToList("krank1"
            , "Wie bist Du Krankenversichert?"
            , ELATYPE$gesetzlichPrivat$key
            , ELACAT$Krankenversicherung
            )
  
  addToList("krank2"
            , "Dein Krankenversicherungsbeitrag ist"
            , ELATYPE$angemessen$key
            , ELACAT$Krankenversicherung
            , list(krank1=list(ELA$privat, ELA$gesetzlich))
            )

  addToList("krankVL"
            , "Die Vorsorgeleistungen meiner Krankenkasse sind mir"
            , ELATYPE$wichtig$key
            , ELACAT$Krankenversicherung
            , list(krank1=list(ELA$privat, ELA$gesetzlich))
            )
  addToList("krankVL2"
            , "Wie zufrieden bist Du mit den Vorsorgeleistungen Deiner Krankenkasse?"
            , ELATYPE$zufrieden$key
            , ELACAT$Krankenversicherung
            , list(krank1=list(ELA$privat, ELA$gesetzlich))
            )

}
getQuestions()

