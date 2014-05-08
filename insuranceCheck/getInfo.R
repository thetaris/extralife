# getInfo: insurance overview
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
hasAny <- function(list) { any(types %in% list) }


# Recommendation 1653: Krankenversicherung
docs <- c(ELTYPE$Gesetzliche.Krankenversicherung, ELTYPE$Private.Krankenversicherung)
recommend("Krankenversicherung angeben", hasAny(docs), term=docs)

docs <- c(ELTYPE$Privathaftpflichtversicherung)
recommend("Haftpflichtversicherung anlegen", hasAny(docs), term=docs)  

docs <- ELTYPE$KFZ.Haftpflichtversicherung
recommend("KFZ-Versicherung anlegen", "TERM", hasAny(docs), term=docs)

docs <- ELTYPE$Lebensversicherung._
recommend("Lebensversicherung", hasAny(docs), term = docs)

docs <- ELTYPE$Hausratversicherung
recommend("Hausratsversicherung anlegen", hasAny(docs), term=docs)

docs <- ELTYPE$Berufsunfaehigkeitsversicherung
recommend("Berunsunfähigkeitsversicherung anlegen", hasAny(docs), term=docs)

if (!hasAny(ELTYPE$Risikolebensversicherung)) {
  for (child in getAll(ELTYPE$Kinder._)) {
    if (child$person.geburtsdatum > "1993-01-01") {
      recommend("Risikolebensversicherung", term=ELTYPE$Risikolebensversicherung)
      break;
    }
  }
}

cat(toJSON(result))