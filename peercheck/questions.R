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
    ELATYPE[[field]]$value<<-range
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
  getElement<-function(row, field){
    res <- elements[row, field]
    if (is.null(res) || is.na(res)){
      eTxt<-sprintf("Error: Could not find '%s' in row %i of Excel file.", field, row+1)
      warning(eTxt)
      return(eTxt)
    }
    res
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
    if (!is.na(elements[iterRow, "requiredAnswers.ID"])){
      ELQuestions[[ID]]$requiredAnswers <<- list()
      requiredAnswers.ID<<-getElement(iterRow, "requiredAnswers.ID")
      if (requiredAnswers.ID %in% names(ELQuestions) ){              
      requiredAnswers.Ans  <<- tryCatch(
        eval(parse(text=sprintf("list%s", elements[iterRow, "requiredAnswers.Ans"])))
        , error=function(e) {
          eTxt<-sprintf("Error: Could not read '%s' as valid requiredAnswers.Ans"
                        ,getElement(iterRow, "requiredAnswers.Ans"))
          warning(eTxt)
          eTxt
          }) 
      
      if (prod(requiredAnswers.Ans %in% ELATYPE[[ELQuestions[[requiredAnswers.ID]]$AType]]$value )){
        ELQuestions[[ID]]$requiredAnswers[requiredAnswers.ID] <<- list(requiredAnswers.Ans)
      }else{
        allowedStr <- paste(unlist(ELATYPE[[ELQuestions[[requiredAnswers.ID]]$AType]]$value), collapse=" ")
        eTxt<-sprintf("Error: Question %s has unknown requiredAnswers.Ans in row %i of Excel file. Allowed values: %s",ID, iterRow+1, allowedStr)
        warning(eTxt)
        ELQuestions[[ID]]$requiredAnswers[requiredAnswers.ID] <<- eTxt
      }
      }else{
        eTxt<-sprintf("Error: Question %s has unknown requiredAnswers.ID %s in row %i of Excel file.",ID, requiredAnswers.ID, iterRow+1)
        warning(eTxt)
        ELQuestions[[ID]]$requiredAnswers[requiredAnswers.ID] <<- eTxt
      }
    }
    
    ELQuestions[[ID]]$priority <<- getElement(iterRow, "priority")
  }
  
}
getQuestions()

