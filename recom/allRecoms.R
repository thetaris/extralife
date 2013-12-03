#
# allRecoms.R
#
# Generate recommendations for document data stored in variable 'json'.

# Resulting list of recommenations
result <- data.frame()

# Generate a recommendation and append it to the result
recommend <- function(recom_id, target_type, target_id, is_satisfied=FALSE) {
  result <<- rbind(result, data.frame(
    recom_id = recom_id,
    type = target_type,
    target_id = as.numeric(target_id),
    is_satisfied = is_satisfied
  ))
}

# Define quick access lists
types = as.numeric(sapply(json, function(iter) {iter$type_id }))
has <- function(type) { any(types == type) }
hasAll <- function(list) { all(sapply(list, function(t) { t %in% types})) }
hasAny <- function(list) { any(sapply(types, function(t) { t %in% list})) }
getAll <- function(list) { json[sapply(types, function(t) { t %in% list})] }
getFields <- function(data, field) { sapply(data, function(iter) {iter[field]})}

#Recommendation 185: Risikolebensversicherung
if (!has(ELTYPE$Risikolebensversicherung)) {
  for (child in getAll(ELTYPE$Kinder._)) {
    if (child$person.geburtsdatum > "1993-01-01") {
      recommend(185, "TERM", ELTYPE$Risikolebensversicherung)
      break;
    }
  }
}

# Recommendation 442: Gueltiges Geburtsdatum
for (node in getAll(ELTYPE$Meine.Familie._)) {
  if (node$person.geburtsdatum=="") {
    recommend(442, "NODE", node$node_id)
  } 
}

# Recommendation 624: Hauptausgaben
ref <- c(ELTYPE$Miete, 
         ELTYPE$Hausgeld, 
         ELTYPE$Kindertagesbetreuung,
         ELTYPE$Hypothek)
if (!hasAny(ref)) {
  for (missing in ref)
    recommend(624, "TERM", missing)
}

# Recommendation 627: Ich Person anlegen
if (!any(types==ELTYPE$Ich)) {
  recommend(627, "TERM", 305)
}

# Recommendation 625: Einkommensart
if (!hasAny(ELTYPE$Einkommen._)) {
  for (missing in ELTYPE$Einkommen._)
    recommend(625, "TERM", missing)
}

# Recommendation 626: Familie anlegen
if (!hasAny(setdiff(ELTYPE$Meine.Familie._, ELTYPE$Ich))) {
  for (missing in ELTYPE$Partner._)
    recommend(626, "TERM", missing)
  for (missing in ELTYPE$Kinder._)
    recommend(626, "TERM", missing)
}

# Recommendation 897: Kaltmiete eintragen
for (node in getAll(ELTYPE$Mietvertrag)) {
  if (node$miete.betrag.kalt=="") {
    recommend(897, "NODE", node$node_id)
  } 
}
