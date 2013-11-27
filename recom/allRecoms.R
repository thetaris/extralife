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
types = as.numeric(sapply(json, function(iter) {iter$type_id }))
has <- function(type) { any(types == type) }
hasAll <- function(type) { all(sapply(types, function(t) { t %in% types})) }
hasAny <- function(type) { any(sapply(types, function(t) { t %in% types})) }

# Recommendation 442: Gueltiges Geburtsdatum
for (node in json) {
  if (sum(node$person.geburtsdatum=="")>0) {
    recommend(442, "NODE", node$node_id)
  } 
}

# Recommendation 628: Ich Person anlegen
if (!any(types==ELTYPE$Ich)) {
  recommend(628, "TERM", 305)
} else {
  # Recommendation 624: Hauptausgaben
  ref <- c(ELTYPE$Miete, 
           ELTYPE$Hausgeld, 
           ELTYPE$Kindertagesbetreuung,
           ELTYPE$Hypothek)
  if (!hasAny(ref)) {
    for (missing in ref)
      recommend(624, "TERM", missing)
  }
  
  # Recommendation 625: Einkommensart
  if (!hasAny(ELTYPE$Einkommen._)) {
    for (missing in ELTYPE$Einkommen._)
      recommend(625, "TERM", missing)
  }
  
  # Recommendation 626: Familie anlegen
  if (!hasAny(setdiff(ELTYPE$Meine.Familie._, ELTYPE$Ich))) {
    for (missing in ELTYPE$Partner._)
      recommend(625, "TERM", missing)
    for (missing in ELTYPE$Kinder._)
      recommend(625, "TERM", missing)
  }
}

print(result)