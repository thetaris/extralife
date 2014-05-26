getELENUM <- function() {
  addToList <- function(field, keys, values){
    tmplist <- list()
    for (iterList in c(1:length(keys))){
      enumName <- keys[iterList]
      enumName <- gsub(' |/|-', '.', enumName)
      enumName <- gsub('\\.*\\(.*\\)', '', enumName)
      enumName <- gsub('\\.+', '.', enumName)
      enumName <- gsub('ä', 'ae', enumName)
      enumName <- gsub('ü', 'ue', enumName)
      enumName <- gsub('ö', 'oe', enumName)
      enumName <- gsub('ß', 'ss', enumName)
      enumName <- gsub(',', '.', enumName)
      
      keyValues <- list()
      keyValues$key <- keys[iterList]
      keyValues$value <- values[iterList]
            
      tmplist[[enumName]] <- keyValues
            
    }    
    ELENUM[[field]] <<- tmplist
  }
  
  
  ELENUM <<- list()
  
  addToList(field = ELFIELD$arbeitsvertrag.beruf
            , keys = c(  "--"
                     , "akademische Tätigkeit"
                     , "Bürotätigkeit"
                     , "leicht körperliche Tätigkeit"
                     , "schwere körperliche Tätigkeit"
                       )
            , values = c(NA, 1,2,3,4)
            ) 
  
  addToList(field = ELFIELD$person.geschlecht
            , keys = c(  "--"
                         , "männlich"
                         , "weiblich"
            )
            , values = c(NA
                         , "mann"
                         ,"frau")
  )
  
  frequencyName <- c('einmalig', 'woche','monat','quartal','halbjahr','jahr')
  frequencyKey <- c('Einmalig', 'Woche','Monat','Quartal','Halbjahr','Jahr')

  addToList(field = ELFIELD$vertrag.zahlung.frequenz
            , keys = frequencyKey
            , values = frequencyName
  )
    
  addToList(field = ELFIELD$vermietung.betrag.frequenz
            , keys = frequencyKey
            , values = frequencyName
  )
    
  addToList(field = ELFIELD$einkommen.betrag.frequenz
            , keys = frequencyKey
            , values = frequencyName
  )
  
  addToList(field = ELFIELD$miete.betrag.frequenz
            , keys = frequencyKey
            , values = frequencyName
  )
  
  preferenceName <- c("Möglichst viel absichern",
                     "Ausgewogene Mischung", 
                     "Sparen, zahle kleine Schäden selbst")
  preferenceKey <- c("viel", "mittel", "wenig")
  
  addToList(field = ELFIELD$ich.risiko.praeferenz
            , keys = preferenceName
            , values = preferenceKey
  )
}

getELENUM()
