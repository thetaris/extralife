require(rjson)

getELTYPE <- function() {
  ELTYPE <<- list()
  
  taxTree <- suppressWarnings(fromJSON(file="http://cloud.thetaris.com/shiny-data/taxonomy-tree")) 
  
  process <- function(subTree) {
    allIds <- c()
    for (taxItem in subTree) {
      # convert special characters in the taxonomy name
      taxName <- taxItem$name
      taxName <- gsub(' |/|-', '.', taxName)
      taxName <- gsub('\\.*\\(.*\\)', '', taxName)
      taxName <- gsub('\\.+', '.', taxName)
      taxName <- gsub('ä', 'ae', taxName)
      taxName <- gsub('ü', 'ue', taxName)
      taxName <- gsub('ö', 'oe', taxName)
      taxName <- gsub('ß', 'ss', taxName)
      
      # record taxonomy entry
      taxId <- as.numeric(taxItem$term_id)
      allIds <- c(allIds, taxId)
      ELTYPE[taxName] <<- taxId
  
      # recurse over children
      if (!is.null(taxItem$children)) {
         subIds <- process(taxItem$children)
         allIds <- c(allIds, subIds)
         groupName <- paste(taxName, '._', sep='')
         ELTYPE[[groupName]] <<- subIds
      }
    }
    return(allIds)
  }
  process(taxTree)
}
getELTYPE()
