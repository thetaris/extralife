require(XLConnect)

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

transformIfWindows<<-function(res){
  #######################################
  # exception for windows: get rid of üäö etc.
  if (Sys.info()[["sysname"]]=="Windows"){
    res<-sapply(res, function(x){x <- gsub(' |/|-', '.', x)
                                 x <- gsub('\\.*\\(.*\\)', '', x)
                                 x <- gsub('\\.+', '.', x)
                                 x <- gsub('€', 'E', x)
                                 x <- gsub('ä', 'ae', x)
                                 x <- gsub('ü', 'ue', x)
                                 x <- gsub('ö', 'oe', x)
                                 x <- gsub('ß', 'ss', x)}  )
    
  }
  #######################################
  res
}

getAType <- function() {  
  ELA <<- list()
  ELATYPE <<- list()
  
  excel.file <- file.path("FragenFormat.xlsx")
  elements <- readWorksheetFromFile(excel.file, sheet=2)
  
  for (iterRow in (1:nrow(elements))){
    field <- elements[iterRow, "field"]
    ELATYPE[[field]] <<- list()
    ELATYPE[[field]]$type <<- elements[iterRow, "field"]
    range <- tryCatch(
      eval(parse(text=sprintf("list%s", elements[iterRow, "range"])))
      , error=function(e) 
        sprintf("Could not read %s as valid range (row %i of excel file)"
                ,elements[iterRow, "range"], iterRow))
    
    range<-transformIfWindows(range)
    
    ELATYPE[[field]]$value<<-transformIfWindows(range)
    ELATYPE[[field]]$key <<- field
    
    type <- elements[iterRow, "type"]
    
    if (type == "enum"){
      for (iterRange in range){
        ELA[[iterRange]] <<- iterRange
      }
    }
  }
  
}

getAType()


getQuestions <- function() {
  getElement<-function(row, field, throwError=T){
    res <- elements[row, field]
    if (throwError && (is.null(res) || is.na(res))){
      eTxt<-sprintf("Error: Could not find '%s' in row %i of Excel file.", field, row+1)
      warning(eTxt)
      return(eTxt)
    }
    transformIfWindows(res)
  }
  
  ELQuestions <<- list()
  
  excel.file <- file.path("FragenFormat.xlsx")
  elements <- readWorksheetFromFile(excel.file, sheet=1)
  
  for (iterRow in (1:nrow(elements))){
    ID <- getElement(iterRow, "ID")
    ELQuestions[[ID]] <<- list()
    ELQuestions[[ID]]$Text <<- getElement(iterRow, "Text")
    ELQuestions[[ID]]$shortText <<- getElement(iterRow, "shortText")
    AType<-getElement(iterRow, "AType")
    if (AType %in% names(ELATYPE)){
      ELQuestions[[ID]]$AType <<- AType
    }else {
      eTxt <- sprintf("Error: Unknown AType '%s' (row %i). Please include in Excel list.", AType, iterRow+1)
      ELQuestions[[ID]]$AType <<- eTxt
      warning(eTxt)
    }
    
    ELQuestions[[ID]]$category <<- getElement(iterRow, "category")
    requiredAnswers.ID<<-getElement(iterRow, "requiredAnswers.ID", throwError=F)
    
    if (!is.na(requiredAnswers.ID)){
      ELQuestions[[ID]]$requiredAnswers <<- list()
      
      if (requiredAnswers.ID %in% names(ELQuestions) ){              
        requiredAnswers.Ans  <- tryCatch(
          eval(parse(text=sprintf("list%s", elements[iterRow, "requiredAnswers.Ans"])))
          , error=function(e) {
            eTxt<-sprintf("Error: Could not read '%s' as valid requiredAnswers.Ans"
                          ,getElement(iterRow, "requiredAnswers.Ans"))
            warning(eTxt)
            eTxt
          }) 
        
        requiredAnswers.Ans<-transformIfWindows(requiredAnswers.Ans)        
        
        possibleAnswers<-( ELATYPE[[ELQuestions[[requiredAnswers.ID]]$AType]]$value )
        
        if (prod(requiredAnswers.Ans %in% possibleAnswers )){
          ELQuestions[[ID]]$requiredAnswers[requiredAnswers.ID] <<- list(requiredAnswers.Ans)
        }else{
          allowedStr <- paste(unlist(possibleAnswers)    , collapse=" ")
          requiredStr<- paste(unlist(requiredAnswers.Ans), collapse=" ")
          eTxt<-sprintf("Error: Question %s has unknown requiredAnswers.Ans (%s) in row %i of Excel file. Allowed values: %s",ID, requiredStr, iterRow+1, allowedStr)
          warning(eTxt)
          ELQuestions[[ID]]$requiredAnswers[requiredAnswers.ID] <<- eTxt
        }
      }else{
        eTxt<-sprintf("Error: Question %s has unknown requiredAnswers.ID %s in row %i of Excel file.",ID, requiredAnswers.ID, iterRow+1)
        warning(eTxt)
        ELQuestions[[ID]]$requiredAnswers[requiredAnswers.ID] <<- eTxt
      }
    }
    
    ELQuestions[[ID]]$priority <<- as.numeric(getElement(iterRow, "priority"))
  }
  
}
getQuestions()

getELGroups <- function() {  
  ELGROUPS <<- list()
  
  excel.file <- file.path("FragenFormat.xlsx")
  elements <- readWorksheetFromFile(excel.file, sheet=3)
  
  for (iRow in (1:nrow(elements))){
    field <- elements[iRow, "ZufriedenheitID"]
    ELGROUPS[[field]] <<- list()
    
    for (iCol in 1:10){      
      question <- elements[iRow, sprintf("Frage%iID", iCol)]
      if (!is.null(question) && !is.na(question)){
        if (question %in% names(ELQuestions)){
          ELGROUPS[[field]] <<- append(ELGROUPS[[field]], question)
        } else{
          eTxt<-sprintf("Error: Group %s has Question %s in row %i of Excel file.",field, question, iRow+1)
          warning(eTxt)
        }
        
      }
      
    }
  }
  
}

getELGroups()
