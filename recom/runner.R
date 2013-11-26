require(rjson, quietly=TRUE)
json=fromJSON(file='stdin')
ELTYPE <- readRDS('recom/ELTYPE.cache')

result <- data.frame()
recommend <- function(recom_id, target_type, target_id, is_satisfied=FALSE) {
  result <<- rbind(result, data.frame(
    recom_id = recom_id,
    type = target_type,
    target_id = as.numeric(target_id),
    is_satisfied = is_satisfied
  ))
}

# Define quick access lists
types = as.numeric(lapply(data, function(iter) {iter$type_id }))


# Recommendation 442: Gueltiges Geburtsdatum
for (node in json) {
  if (sum(node$person.geburtsdatum=="")>0) {
    recommend(442, "NODE", node$node_id)
  } 
}

# Recommendation 505: Berufsunfaehigkeit
recommend(505, "TERM", 266)

# Recommendation 628: Ich Person anlegen
if (!any(types==ELTYPE$ich)) {
  recommend(628, "TERM", 305)
}


cat(toJSON(result))



