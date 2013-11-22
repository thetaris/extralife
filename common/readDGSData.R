allDGSDataFields <- function(){
  return(tolower(c(    
  "node_id", 
  "type_id",
  "title",
  "person.geschlecht",
  "person.geburtsdatum",
  "adresse", 
  "Kauf.datum",
  "Kauf.wert",
  "Hersteller",
  "Vertrag.nummer",
  "Vertrag.Zahlung.betrag",
  "Vertrag.Zahlung.betrag.konsum",
  "Vertrag.Zahlung.betrag.investition",
  "Vertrag.Zahlung.start",
  "Vertrag.Zahlung.frequenz",
  "Verttrag.Zahlung.ende",
  "Mietvertrag.Mieter",
  "Mietvertrag.Adresse",
  "Vermietung.betrag.kalt",
  "Vermietung.betrag.nebenkosten",
  "Vermietung.betrag.start",
  "Vermietung.betrag.frequenz",
  "Vermietung.betrag.ende",
  "Einkommen.betrag.brutto",
  "Einkommen.betrag.netto",
  "Einkommen.betrag.frequenz",
  "Einkommen.betrag.start",
  "Einkommen.betrag.ende",
  "Arbeitsvertrag.Arbeitgeber",
  "Arbeitsvertrag.Zeitanteil",
  "Rente.Traeger",
  "Mietvertrag.Vermieter",
  "Miete.betrag.kalt",
  "Miete.betrag.nebenkosten",
  "Miete.betrag.start",
  "Miete.betrag.frequenz",
  "Miete.betrag.ende",
  "Zeitwert.betrag",
  "Zeitwert.datum",
  "Versicherung.tarif",
  "gesetzlicheRente.entgeldpunkte.anzahl",
  "gesetzlicheRente.entgeldpunkte.datum",
  "Versicherung.Deckungssumme.betrag",
  "Versicherung.Selbstbeteiligung",
  "Versicherung.Ausfalldeckung",
  "Versicherung.Haftpflicht.Schluesselverlust",
  "Invaliditaet.Rente.betrag",
  "Invaliditaet.Rente.bezugsende",
  "Bu.VersicherterBeruf",
  "Bankkonto.IBAN",
  "KFZVersicherung.Modell",
  "KFZVersicherung.Hersteller",
  "PrivateRente.Grundsumme",
  "PrivateRente.Progression",
  "Versicherung.Todesfallleistung",
  "Versicherung.Unfallrente",
  "Schadensabdeckung.privat",
  "Schadensabdeckung.beruf",
  "Schadensabdeckung.verkehr",
  "Schadensabdeckung.wohnen",
  "Schadensabdeckung.vermietung",
  "Schadensabdeckung.Blitzschlag",
  "Schadensabdeckung.sturm",
  "Schadensabdeckung.Hagel",
  "Schadensabdeckung.Leitungswasser",
  "Schadensabdeckung.Elementar",
  "Auto.Kennzeichen",
  "Auto.Schluesselnummer",
  "Auto.Fahrzeugidentnummer",
  "Boot.klasse",
  "Motorrad.klasse",
  "Fahrrad.rahmennummer",
  "Immobilie.wohnflaeche",
  "Immobilie.grundstücksflaeche",
  "Immobilie.grundbuchnummer",
  "Pferd.Equidenpass",
  "Hund.Hundemarke",
  "Bausparen.Tarif",
  "Bausparen.Summe",
  "Bausparen.MindestSparGuthaben",
  "kommentarfeld")))  
}

getType_idFromTaxonomyMap <- function(){
  ich <- 305
  Ehefrau <- 295
  Ehemann <-294
  Lebenspartner	<- 296
  
  Tochter<-298
  Sohn<-299
  
  Meine_Mutter<-302
  Mein_Vater<-301
  Schwiegermutter<-304
  Schwiegervater<-303
  
  Partner = c(ich, Ehefrau, Ehemann, Lebenspartner)
  Kinder = c(Tochter, Sohn)
  Eltern = c(Meine_Mutter, Mein_Vater, Schwiegermutter, Schwiegervater)

  Meine_Familie = c(Partner, Kinder, Eltern)
  
  result = list(ich=ich, 
                Ehefrau=Ehefrau, 
                Ehemann=Ehemann,
                Lebenspartner=Lebenspartner, 
                Tochter = Tochter, 
                Sohn = Sohn, 
                Meine_Mutter=Meine_Mutter,
                Mein_Vater=Mein_Vater,
                Schwiegermutter=Schwiegermutter,
                Schwiegervater=Schwiegervater,
                
                Partner = Partner,
                Kinder = Kinder,
                Eltern = Eltern,
                
                Meine_Familie = Meine_Familie
  )
  
  return(result)
}

readDGSData <- function(requestedFields, session = NULL, sid = NULL, file = NULL){
# return data.frame of requested fields from DGS server  
# 
# deliver all available data:
# readDGSData(allDGSDataFields(), sid = "abc") 
#
# deliver title
# readDGSData(requestedFields=c('title'), file = "../test/testdata.json") 
  
  if(is.null(file)){
    if (is.null(session))
    {
      if (is.null(sid)){
        stop("Either 'session' or 'sid' has to be supplied as argument.")           
      }
    } else {    
      sid <- sub('^.*sid=([a-zA-Z_/0-9-]*).*$', '\\1', session$clientData$url_search, fixed=FALSE)
    }
    file <- paste("http://cloud.thetaris.com/shiny-data/alldata?sid=",sid,sep='') 
  }
  
  data <- fromJSON(file=file)
  
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
#   dataObj <- DGSData(file = "../test/testdata.json" )
#   tmp = dataObj$get("person.geschlecht")
#   tmp = dataObj$get("person.geburtsdatum")
#   tmp = dataObj$get("person.geburtsdatum", getType_idFromTaxonomyMap()$Meine_Familie)
#   tmp = dataObj$get("person.geburtsdatum", getType_idFromTaxonomyMap()$ich)
#   print(dataObj$getLog())
#   #
  
  resultObj <- new.env()
  
  # get all data from session
  resultObj$.data <- readDGSData(allDGSDataFields(), session = session, sid = sid, file = file)
  
  # log used data: [node_id, title, field, value, estimatedFlag]
  resultObj$.dataLog <- data.frame(matrix(NA, nrow = 0, ncol = 6))
  
  op <- options(digits.secs = 3)
  
  resultObj$get <- function(requestedField, type = NULL, node_id = NULL){    
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
    
    # check inputs    
    if (!is.null(type)){
      # type_id is provided: find matching nodes
      if (!is.null(node_id)){
        stop("DGSData$get cannot take type and node_id togther. Choose one.")
      }
      sel = sapply(.data[,"type_id"],function(x) sum(x == type)>0)      
      node_id <- .data[sel,"node_id"]
    }
    
    if (sum(allDGSDataFields() == requestedField) == 0){
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
    
    node_id       <- sanitize(.data[,"node_id"], n)
    title         <- sanitize(.data[,"title"], n)
    field         <- sanitize(requestedField, n)  
    
    estimatedFlag <- sanitize(FALSE, n)
    
    if (sys.nframe()>1){
      caller        <- rep(as.character(deparse(sys.calls()[[sys.nframe()-1]])),n)
      
    }else{
      caller <- rep('console', n)
    }      
    
    timeStamp    <- sanitize(format(Sys.time(), "%y-%m-%d %H:%M:%OS"),n)
    

    
    tmp_data = data.frame(node_id = node_id, title = title, field = field, value = value, estimatedFlag = estimatedFlag, caller = caller, timeStamp = timeStamp)
    
    .dataLog <<- rbind(.dataLog, tmp_data)
    
    return(value)
  }
  environment(resultObj$get) <- as.environment(resultObj)
  
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
  taxTree = fromJSON(file="http://cloud.thetaris.com/shiny-data/taxonomy-tree") 
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
  
  wert = as.numeric(dataObj$get("zeitwert.betrag"))
  
  bewertung = wert
  bewertung[!is.na(bewertung)] <- "static"
  
  tmp_wert1 = as.numeric(dataObj$get("vertrag.zahlung.betrag"))
  tmp_wert2 = as.numeric(dataObj$get("vertrag.zahlung.betrag.konsum"))
  tmp_wert3 = as.numeric(dataObj$get("vertrag.zahlung.betrag.investition"))
  
  tmp_wertAll = tmp_wert1
  tmp_wertAll[!is.na(tmp_wert2)]<-tmp_wert2[!is.na(tmp_wert2)]
  tmp_wertAll[!is.na(tmp_wert3)]<-tmp_wert3[!is.na(tmp_wert3)]
  
  bewertung[!is.na(tmp_wertAll)] <- "expense"
  wert[!is.na(tmp_wertAll)] <- tmp_wertAll[!is.na(tmp_wertAll)]
  
  bewertung[taxonomy2 == "Kredit"] <- "credit"
  
  
  tmp_wertEinkommen = as.numeric(dataObj$get("einkommen.betrag.netto"))  
  bewertung[!is.na(tmp_wertEinkommen)] <- "income"
  wert[!is.na(tmp_wertEinkommen)] <- tmp_wertEinkommen[!is.na(tmp_wertEinkommen)]
  
  data.frame(titel, taxonomy1, taxonomy2, taxonomy3, wert, bewertung)
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