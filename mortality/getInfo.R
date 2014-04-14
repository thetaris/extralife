# getInfo Mortality Report
require(rjson, quietly=TRUE)

json=fromJSON(file='stdin')
#json=fromJSON('[{"node_id":"123","type_id":305,"title":"test"}]')

result <- list(recom = list())
recommend <- function(text, status=FALSE, node=NULL, term=NULL) {
  recom <- list(text = text, 
                  status = status,
                  about_node = node,
                  add_term = term)
  result$recom <<- rbind(result$recom, list(recom))
}

types = as.numeric(sapply(json, function(iter) {iter$type_id }))
getAll <- function(list) { json[types %in% c(list)] }

recommend("Eigene Person anlegen.", any(types==ELTYPE$Ich), term=305)

# Gueltiges Geburtsdatum
for (node in getAll(ELTYPE$Meine.Familie._)) {
  if (is.null(node$person.geburtsdatum) ||
        !grepl('....-..-..',node$person.geburtsdatum)) {
    recommend("Gültiges Geburtsdatum angeben", node = node$node_id)
  } 
}


# Data access
dataaccess <- list()
dataaccess$fields <- list()
dataaccess$fields[[ELFIELD$person.geschlecht]] = ELTYPE$Ich
dataaccess$fields[[ELFIELD$person.geburtsdatum]] = ELTYPE$Meine.Familie._

dataaccess$types = ELTYPE$Meine.Familie._
result$dataaccess <- dataaccess

cat(toJSON(result))
fields <-list()
fields[[ELFIELD$person.geschlecht]] = ELTYPE$Ich
fields[[ELFIELD$person.geburtsdatum]] = ELTYPE$Ich

data <- list(types = c(ELTYPE$Meine.Familie, ELTYPE$Ich),
             fields = fields)