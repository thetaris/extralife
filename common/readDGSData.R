source("../common/getELTYPE.R", encoding="UTF-8")
source("../common/getELFIELD.R")


readDGSData <- function(requestedFields, session = NULL, sid = NULL, file = NULL){
  # return data.frame of requested fields from DGS server  
  # 
  # deliver all available data:
  # readDGSData(ELFIELD$._, sid = "abc") 
  #
  # deliver title
  # readDGSData(requestedFields=c('title'), file = "../test/data/test_simpson_Familie.json") 
  
  if(is.null(file)){
    if (is.null(session))
    {
      if (is.null(sid)){
        stop("Either 'session' or 'sid' has to be supplied as argument.")           
      }
    } else {    
      sid <- sub('^.*sid=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE)
    }   
    
    if ((length(session$clientData$url_search)>0) && (sid==session$clientData$url_search)){
      testFile <- sub('^.*test=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE)
      file <- sprintf("../test/data/%s.json",testFile) 
    } else {
      file <- paste("http://cloud.thetaris.com/shiny-data/alldata?sid=",sid,sep='') 
    }
  }
  print(file)
  data <- suppressWarnings(fromJSON(file=file))
  
  # fill non-existent fields with NULL  
  data2 <- lapply(data, function(j) { j[requestedFields] })
  
  # merge all rows into one big matrix  
  result <- Reduce(rbind, data2, c())
  
  # rename requested fields such that they are legal column names
  #newColnames <- gsub("_", "", requestedFields)
  newColnames <- requestedFields
  
  # reset the names of the headers  
  colnames(result) <- newColnames
  
  return(result)
}

DGSData <- function(session = NULL, sid = NULL, file = NULL){
  # Creating a dataobject with methods for reading data
  #
  # example:
  #  
  #   dataObj <- DGSData(file = "../test/data/test_simpson_Familie.json" )
  #   tmp = dataObj$get(ELFIELD$person.geschlecht)
  #   tmp = dataObj$get(ELFIELD$person.geburtsdatum)
  #   tmp = dataObj$get(ELFIELD$person.geburtsdatum, type=ELTYPE$Meine.Familie._)
  #   tmp = dataObj$get(ELFIELD$person.geburtsdatum, type=ELTYPE$Ich)
  #   print(dataObj$getLog())
  #   #
  
  resultObj <- new.env()
  
  # get all data from session
  resultObj$.data <- readDGSData(ELFIELD$._, session = session, sid = sid, file = file)
  
  # log used data: [node_id, title, field, value, estimatedFlag]
  resultObj$.dataLog <- data.frame(matrix(NA, nrow = 0, ncol = 7))
  
  op <- options(digits.secs = 3)
  
  resultObj$get <- function(requestedField, type = NULL, node_id = NULL){        
    toMonthly <- function(betrag, freq){
      if (is.null(betrag)){
        return(NULL)
      }else{
        freq[freq==""]="none"
        freq[is.na(freq)]="none"      
        cost <- numeric(length(freq)) 
        if (length(freq)>0){
          for (iterDoc in 1:length(freq)){
            if (is.na(betrag[iterDoc])){
              cost[iterDoc]<-NA
            } else {
              switch(freq[iterDoc]
                     ,einmalig={cost[iterDoc]<-0}
                     ,woche   ={cost[iterDoc]<-4.3*betrag[iterDoc]}
                     ,monat   ={cost[iterDoc]<-betrag[iterDoc]}
                     ,quartal ={cost[iterDoc]<-betrag[iterDoc]/3}
                     ,halbjahr={cost[iterDoc]<-betrag[iterDoc]/6}
                     ,jahr    ={cost[iterDoc]<-betrag[iterDoc]/12}
                     ,none    ={cost[iterDoc]<-betrag[iterDoc]} # assume monthly
                     ,stop(sprintf("Error computing i.kosten.monatlich: Incorrect frequency :%s in document of type %s", freq[iterDoc], type))
              )
            }
          }
        }else{
          cost <- NULL
        }
        tmpres<-list()
        tmpres$value <- cost
        return(tmpres)
      }
    } # end toMonthky
    
    getMonthly<-function(field, freq, type, node_id){
      betrag <- suppressWarnings(resultObj$get_raw(requestedField=field, type, node_id))
      betrag <- as.numeric(betrag$value)
      
      freqenz<-resultObj$get_raw(requestedField=freq, type, node_id)
      freqenz <- freqenz$value  
      tmpres<-toMonthly(betrag, freqenz)
      return(tmpres)
    } # end getMonthly
    
    addnoNA<-function(listSum){
      if (is.null(listSum)){
        return(null)
      }else{
        tmp<-listSum[[1]]
        for (iterList in listSum){
          
          sel<-is.na(tmp$value)
          tmp$value[sel]<-iterList$value[sel]
          sel_plus <- (!sel)&(!is.na(iterList$value))
          tmp$value[sel_plus]<-tmp$value[sel_plus] + iterList$value[sel_plus]
        }
      }
      return(tmp)
    }# end addnoNA
    
    if (requestedField==ELFIELD$i.kosten.monatlich){
            
      konsum            <- getMonthly(field=ELFIELD$vertrag.zahlung.betrag.konsum
                                      , freq=ELFIELD$vertrag.zahlung.frequenz, type, node_id)
      investition       <- getMonthly(field=ELFIELD$vertrag.zahlung.betrag.investition
                                      , freq=ELFIELD$vertrag.zahlung.frequenz, type, node_id)
      miete             <- getMonthly(field=ELFIELD$miete.betrag.kalt
                                      , freq=ELFIELD$miete.betrag.frequenz, type, node_id)
      miete_nebenkosten <- getMonthly(field=ELFIELD$miete.betrag.nebenkosten
                                      , freq=ELFIELD$miete.betrag.frequenz, type, node_id)
                    
      
      res<-addnoNA(list(konsum, investition, miete, miete_nebenkosten))
      # not logged yet
    }else if (requestedField==ELFIELD$i.einkommen.monatlich){
      brutto            <- getMonthly(field=ELFIELD$einkommen.betrag.brutto
                                     , freq=ELFIELD$einkommen.betrag.frequenz, type, node_id)

      netto            <- getMonthly(field=ELFIELD$einkommen.betrag.netto
                                     , freq=ELFIELD$einkommen.betrag.frequenz, type, node_id)
      
      einkommen <- list()
      einkommen$value <- netto$value
      einkommen$value[is.na(einkommen$value)]<-brutto$value[is.na(einkommen$value)]
      
      
      res<-einkommen
    }   
    else{
      res <- resultObj$get_raw(requestedField, type, node_id)
      
      # infer gender from type
      if (requestedField==ELFIELD$person.geschlecht){
        women <- ((res$type==ELTYPE$Ehefrau) 
                  | (res$type==ELTYPE$Lebenspartnerin)
                  | (res$type==ELTYPE$Tochter)
                  | (res$type==ELTYPE$Mutter)
                  | (res$type==ELTYPE$Schwiegermutter)
        )
        
        men <-  ((res$type==ELTYPE$Ehemann)
                 | (res$type==ELTYPE$Lebenspartner)
                 | (res$type==ELTYPE$Sohn)
                 | (res$type==ELTYPE$Vater)
                 | (res$type==ELTYPE$Schwiegervater)
        )
        
        res[women,"value"] <- "frau"
        res[women,"estimatedFlag"] <- "correct"
        res[men,"value"] <- "mann"      
        res[men,"estimatedFlag"] <- "correct"
      }
      .dataLog <<- rbind(.dataLog, res)
    }
    return(res$value)
  } # end $get
  
  resultObj$get_raw <- function(requestedField, type = NULL, node_id = NULL){    
    # define a helper function
    sanitize <- function(data_in, n){
      # format data_in such that it can be used creating a data.frame
      if (is.list(data_in)){
        # replace NULL by NA to prevent error in data.frame
        data_in[unlist(lapply(data_in,is.null))] <- NA
        result <- unlist(data_in)
      }
      
      if (length(data_in) == 1){
        result <- rep(data_in,n)
      }
      
      return(result)
    }
    sel = NULL
    # check inputs    
    if (!is.null(type)){
      # type_id is provided: find matching nodes
      if (!is.null(node_id)){
        stop("DGSData$get cannot take type and node_id togther. Choose one.")
      }
      sel = sapply(.data[,"type_id"],function(x) sum(x == type)>0)      
      node_id <- .data[sel,"node_id"]
    }
    
    if (sum(ELFIELD$._ == requestedField) == 0){
      stop(sprintf("DGSData$get() : requestedField '%s' does not exist", requestedField))
    }
    
    if (is.null(node_id)){
      value <- sanitize(.data[,requestedField])
    } 
    else {
      sel = sapply(.data[,"node_id"],function(x) sum(x == node_id)>0) 
      value <- sanitize(.data[sel,requestedField])
    }
    
    n <- length(value)
    
    if (is.null(sel)){
      node_id       <- sanitize(.data[,"node_id"], n)
      title         <- sanitize(.data[,"title"], n)
      field         <- sanitize(requestedField, n)
      type          <- sanitize(.data[,"type_id"], n)
    } else{
      node_id       <- sanitize(.data[sel,"node_id"], n)
      title         <- sanitize(.data[sel,"title"], n)      
      field         <- sanitize(requestedField, n) 
      type          <- sanitize(.data[sel,"type_id"], n)
    }
    estimatedFlag <- sanitize(FALSE, n)
    
#     does not work yet!!!
#
#     if (sys.nframe()>1){
#       caller        <- rep(as.character(deparse(sys.calls()[[sys.nframe()-1]])),n)
#       caller        <- caller[1:n]      
#     }else{
#       caller <- rep('console', n)
#     }   
#     
    ##############?????
    caller <- title
    
    timeStamp    <- sanitize(format(Sys.time(), "%y-%m-%d %H:%M:%OS"),n)    
    
    
    tmp_data = data.frame(node_id, title, field, type, value, estimatedFlag, caller, timeStamp, stringsAsFactors=FALSE)
    try(colnames(tmp_data)<- c("node_id", "title","field", "type","value" ,"estimatedFlag", "caller", "timeStamp"), silent = TRUE)          
    
    return(tmp_data)
  }
  environment(resultObj$get) <- as.environment(resultObj)
  environment(resultObj$get_raw) <- as.environment(resultObj)
  
  resultObj$getLog <- function(requestedField){        
    return(.dataLog)
  }
  environment(resultObj$getLog) <- as.environment(resultObj)
  
  
  # might help according to http://www.lemnica.com/esotericR/Introducing-Closures/
  #class(resultObj) <- "DGSData"
  
  return(resultObj)
}


getCashItm<-function (dataObj = NULL, file = NULL){
  titel <- character()
  taxonomy1 <- character()
  taxonomy2 <- character()
  taxonomy3 <- character()
  wert <- numeric()
  zahlungsbeginn <- date()
  zahlungsbetrag <- numeric()
  zahlungswiederholung <- character()
  zahlungsende <- date()
  bewertung <- character()
  
  # read Taxonomy
  taxTree = suppressWarnings(fromJSON(file="http://cloud.thetaris.com/shiny-data/taxonomy-tree"))
  dat<-getTaxonomy(taxTree, recursive = TRUE)
  
  if (is.null(dataObj)){
    dataObj <- DGSData(file = file)
  }
  
  titel = dataObj$get("title")
  
  taxonomy = as.numeric(dataObj$get("type_id"))
  
  for (iterTax in taxonomy){
    tmp = as.character(dat[dat$type_id==iterTax,"name"])
    tmp = sub("_", " ", tmp)
    taxonomy1 = c(taxonomy1, tmp[1])
    taxonomy2 = c(taxonomy2, tmp[2])
    if (length(tmp)<3) {
      tmp[3] <- NA
    }
    taxonomy3 = c(taxonomy3, tmp[3])
  }
  # Active balance: value
  wert = round(as.numeric(dataObj$get(ELFIELD$zeitwert.betrag)),2)
  bewertung = wert
  bewertung[!is.na(bewertung)] <- "static"
  res_static <- data.frame(titel, taxonomy1, taxonomy2, taxonomy3, wert, bewertung, stringsAsFactors=F)
  
  # Cost: expense
  wert = round(as.numeric(dataObj$get(ELFIELD$i.kosten.monatlich)),2)
  bewertung = wert
  bewertung[!is.na(bewertung)] <- "expense"  
  res_expense <- data.frame(titel, taxonomy1, taxonomy2, taxonomy3, wert, bewertung, stringsAsFactors=F)
  
  # Passive balance: credit
  wert = round(as.numeric(dataObj$get(ELFIELD$kredit.zeitwert.betrag)),2)
  bewertung = rep(NA,length(wert))
  bewertung[taxonomy2 == "Kredit"] <- "credit"
  res_credit <- data.frame(titel, taxonomy1, taxonomy2, taxonomy3, wert, bewertung, stringsAsFactors=F)
  
  # Earnings: income
  wert = round(as.numeric(dataObj$get(ELFIELD$i.einkommen.monatlich)),2)
  bewertung = wert
  bewertung[!is.na(bewertung)] <- "income"  
  res_income <- data.frame(titel, taxonomy1, taxonomy2, taxonomy3, wert, bewertung, stringsAsFactors=F)    
  
  res <- rbind(res_static, res_expense, res_credit, res_income)
  res <- res[!is.na(res$bewertung),]
  res[is.na(res$wert),"wert"]<-0
  return(res)
}


getTaxonomy <- function(taxTree, recursive = FALSE){
  # returns a data.frame with a map of type_id and taxonomy name
  # set the argument "recursive" to TRUE for inclusion of the parents
  #
  # example:
  #
  # > taxTree = fromJSON(file="http://cloud.thetaris.com/shiny-data/taxonomy-tree") 
  # > dat<-getTaxonomy(taxTree, recursive = TRUE)
  #   
  # > dat[dat$type_id==305,]
  #        type_id          name
  #     12     305 Meine_Familie
  #     16     305       Partner
  #     20     305           ich
  #  
  # > dat[dat$name=="ich",]
  #        type_id name
  #     20     305  ich
  #
  # author: Andreas
  
  res = data.frame(numeric(0), character(0))
  colnames(res) <- c("type_id", "name")
  
  
  name = sapply(taxTree, function(x) x$name)
  name = sub(" ", "_", name)
  
  type_id <- as.numeric(sapply(taxTree, function(x) x$term_id))
  
  res = rbind(res, data.frame(type_id, name))
  
  
  # level 0 taxonomies
  if(length(name)>0){
    for (iterName in 1:length(name)){
      # level 1 taxonomies
      taxTree1 <- taxTree[[iterName]]$children
      
      name1 = sapply(taxTree1, function(x) x$name)
      name1 = sub(" ", "_", name1)
      
      type_id1 <- as.numeric(sapply(taxTree1, function(x) x$term_id))
      
      if (recursive){
        res0 <- data.frame(type_id1, rep(name[iterName], length(name1)))
        colnames(res0) <-  c("type_id", "name")
        res = rbind(res, res0)
      }
      res1 <- data.frame(type_id1, name1)
      colnames(res1) <-  c("type_id", "name")
      res = rbind(res, res1)
      
      # level 2 taxonomies
      if(length(name1)>0){
        
        for (iterName1 in 1:length(name1)){
          taxTree2 <- taxTree1[[iterName1]]$children
          
          name2 = sapply(taxTree2, function(x) x$name)
          name2 = sub(" ", "_", name2)
          
          type_id2 <- as.numeric(sapply(taxTree2, function(x) x$term_id))
          
          if (recursive) {
            res0 <- data.frame(type_id2, rep(name[iterName], length(name2)))
            colnames(res0) <-  c("type_id", "name")
            res = rbind(res, res0)
            
            res1 <- data.frame(type_id2, rep(name1[iterName1], length(name2)))
            colnames(res1) <-  c("type_id", "name")
            res = rbind(res, res1)
          }
          res2 <- data.frame(type_id2, name2)
          colnames(res2) <-  c("type_id", "name")
          res = rbind(res, res2)
        }
      }
    }
  }
  return(res)
  
}