getELACAT <- function(){
  allELACAT <- function(){
    return(c(    
      "Krankenversicherung"
      , "Lebenszufriedenheit"
      , "Autoversicherung"
      , "Biometrie"))  
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
  addToList("alter", "enum", sprintf("%i bis %i", seq(10, 80, 10), seq(19, 89, 10)) ) 
  addToList("geschlecht",  "enum", list("Mann", "Frau"))
  addToList("beziehungsstatus",  "enum", list("Single"
                                              , "in einer Beziehung"
                                              , "verlobt"
                                              , "verheiratet"
                                              , "in einer offenen Beziehung"
                                              , "es ist kompliziert"
                                              , "getrennt"
                                              , "geschieden"
                                              , "verwitwet"))
  addToList("anzKinder", "enum", list("keines", "1", "2", "3", "4 oder mehr"))
  addToList("alterKinder", "enum", list("0 bis 2", "3 bis 5", "6 bis 11", "12 bis 17", "18 oder älter"))
}

getAType()



getQuestions <- function() {
  addToList <- function(ID, Text, shortText, ATypeKey, category, requiredAnswers = NULL, priority){
    ELQuestions[[ID]] <<- list()
    ELQuestions[[ID]]$Text <<- Text
    ELQuestions[[ID]]$shortText <<- shortText
    ELQuestions[[ID]]$AType <<- ATypeKey
    ELQuestions[[ID]]$category <<- category
    ELQuestions[[ID]]$requiredAnswers <<- requiredAnswers
    ELQuestions[[ID]]$priority <<- priority
  }
  
  ELQuestions <<- list()
  addToList("lebenszufrieden"
            , "Wie zufrieden bist Du mit Deinem Leben?"
            , "Wie zufrieden?"
            , ELATYPE$zufrieden$key
            , ELACAT$Lebenszufriedenheit
            , priority = 1000
  )
  
  addToList("krank1"
            , "Wie bist Du Krankenversichert?"
            , "Wie krankenversichert?"
            , ELATYPE$gesetzlichPrivat$key
            , ELACAT$Krankenversicherung
            , priority = 100
            )
  
  addToList("krank2"
            , "Dein Krankenversicherungsbeitrag ist"
            , "Welchen Krankenversicherungbeitrag?"
            , ELATYPE$angemessen$key
            , ELACAT$Krankenversicherung
            , list(krank1=list(ELA$privat, ELA$gesetzlich))
            , priority = 100
            )

  addToList("krankVL"
            , "Die Vorsorgeleistungen meiner Krankenkasse sind mir"
            , "Ist Vorsorge wichtig?"
            , ELATYPE$wichtig$key
            , ELACAT$Krankenversicherung
            , list(krank1=list(ELA$privat, ELA$gesetzlich))
            , priority = 100
            )
  addToList("krankVL2"
            , "Wie zufrieden bist Du mit den Vorsorgeleistungen Deiner Krankenkasse?"
            , "Zufrieden mit Vorsorge?"
            , ELATYPE$zufrieden$key
            , ELACAT$Krankenversicherung
            , list(krank1=list(ELA$privat, ELA$gesetzlich))
            , priority = 100
            )
  addToList("alter"
            , "Wie alt bist Du?"
            , "Wie alt?"
            , ELATYPE$alter$key
            , ELACAT$Biometrie
            , priority = 200
            )
  
  addToList("geschlecht"
            , "Bist du ein Mann oder eine Frau?"
            , "Mann oder Frau?"
            , ELATYPE$geschlecht$key
            , ELACAT$Biometrie
            , priority = 199
  )
  
  addToList("beziehung"
            , "Wie würdest Du deinen Beziehungsstatus am ehesten beschreiben?"
            , "Beziehungsstatus?"
            , ELATYPE$beziehungsstatus$key
            , ELACAT$Biometrie
            , priority = 100            
  )
  
  addToList("anzKinder"
            , "Wie viele Kinder hast Du?"
            , "Anzahl Kinder?"
            , ELATYPE$anzKinder$key
            , ELACAT$Biometrie
            , priority = 100            
  )
  
  addToList("alterKinder"
            , "Wie alt ist Dein ältestes Kind?"
            , "Alter ältestes Kind?"
            , ELATYPE$alterKinder$key
            , ELACAT$Biometrie
            , list(anzKinder=list(ELA$"1", ELA$"2", ELA$"3", ELA$"4 oder mehr"))
            , priority = 1000            
  )
  
  addToList("alterKinder"
            , "Wie alt ist Dein jüngstes Kind?"
            , "Alter jüngstes Kinder?"
            , ELATYPE$alterKinder$key
            , ELACAT$Biometrie
            , list(anzKinder=list(ELA$"2", ELA$"3", ELA$"4 oder mehr"))
            , priority = 1000            
  )
  
  
}
getQuestions()

