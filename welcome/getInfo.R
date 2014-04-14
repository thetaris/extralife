# getInfo Welcome App
require(rjson, quietly=TRUE)

json=fromJSON(file='stdin')
#json=fromJSON('[{"node_id":"123","type_id":305,"title":"test"}]')

result <- list(recom = list())
recommend <- function(text, status=FALSE, node=NULL, term=NULL) {
  recom <- list(text = text, 
                  status = is.null(node) && is.null(term),
                  about_node = node,
                  add_term = term)
  result$recom <<- rbind(result$recom, list(recom))
}

types = as.numeric(sapply(json, function(iter) {iter$type_id }))
getAll <- function(list) { json[types %in% c(list)] }

# Icon HTML
if (any(types==ELTYPE$Ich)) {
  result$iconHtml <- '<img src="/sites/default/files/Logo_Welcome.png" width="100%">'
} else {
  result$iconHtml <- 'Keine Daten.'
}

#Recommendations
types = as.numeric(sapply(json, function(iter) {iter$type_id }))
getAll <- function(list) { json[types %in% c(list)] }
hasAny <- function(list) { any(types %in% list) }

recommend("Eigene Person anlegen.", any(types==ELTYPE$Ich), term=305)

# Gueltiges Geburtsdatum
for (node in getAll(ELTYPE$Ich)) {
  if (is.null(node$person.geburtsdatum) ||
        !grepl('....-..-..',node$person.geburtsdatum)) {
    recommend("Gültiges Geburtsdatum angeben", node = node$node_id)
  } 
}

fields <-list()
fields[[ELFIELD$person.geschlecht]] = ELTYPE$Ich
fields[[ELFIELD$person.geburtsdatum]] = ELTYPE$Ich

result$dataaccess <- list(types = c(ELTYPE$Meine.Familie, ELTYPE$Ich), fields = fields)



cat(toJSON(result))
