require(rjson, quietly=TRUE)
json=fromJSON(file='stdin')
#json=fromJSON('[{"node_id":"123","type_id":305,"title":"test"}]')
ELTYPE <- readRDS('recom/ELTYPE.cache')

result <- list(recom = list())
recommend <- function(text, status, node=NULL, term=NULL) {
  recom <- list(text = text, 
                  status = is.null(node) && is.null(term),
                  about_node = node,
                  add_term = term)
  result$recom <<- rbind(result$recom, list(recom))
}

types = as.numeric(sapply(json, function(iter) {iter$type_id }))
getAll <- function(list) { json[types %in% c(list)] }

if (any(types==ELTYPE$Ich)) {
  result$iconHtml <- '<img src="/sites/default/files/Logo_Welcome.png" width="100%">'
  recommend("Eigene Person anlegen.")
} else {
  result$iconHtml <- 'Keine Daten.'
  recommend("Eigene Person anlegen.", term=305)
}

# Gueltiges Geburtsdatum
#for (node in getAll(ELTYPE$Meine.Familie._)) {
for (node in getAll(ELTYPE$Ich)) {
  if (is.null(node$person.geburtsdatum) ||
        !grepl('....-..-..',node$person.geburtsdatum)) {
    recommend("Gültiges Geburtsdatum angeben", node = node$node_id)
  } 
}

cat(toJSON(result))
