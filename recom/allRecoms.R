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
recommendTerms <- function(recom_id, term_list) {
  for (term in term_list)
    recommend(recom_id, "TERM", term)
}

# Define quick access lists
types = as.numeric(sapply(json, function(iter) {iter$type_id }))
has <- function(type) { any(types == type) }
hasAll <- function(list) { all(list %in% types) }
hasAny <- function(list) { any(types %in% list) }
getAll <- function(list) { json[types %in% c(list)] }
getFields <- function(data, field) { sapply(data, function(iter) {iter[field]})}

# Recommendation 627: Ich Person anlegen
if (!any(types==ELTYPE$Ich)) {
  recommend(627, "TERM", 305)
  recommend(1650, "NODE", 0)
  recommend(1651, "NODE", 0)
  recommend(442, "NODE", 0)
} else {
  for (ich in getAll(ELTYPE$Ich)) {
  if (ich$title=="ich" || ich$title=="")
    recommend(1650, "NODE", ich$node_id)
  if (ich$person.geschlecht=="")
    recommend(1651, "NODE", ich$node_id)
  }
}

# Recommendation 442: Gueltiges Geburtsdatum
for (node in getAll(ELTYPE$Meine.Familie._)) {
  if (!grepl('....-..-..',node$person.geburtsdatum)) {
    recommend(442, "NODE", node$node_id)
  } 
}

# Recommendation 626: Familie anlegen
if (!hasAny(setdiff(ELTYPE$Meine.Familie._, ELTYPE$Ich))) {
  for (missing in ELTYPE$Partner._)
    recommend(626, "TERM", missing)
  for (missing in ELTYPE$Kinder._)
    recommend(626, "TERM", missing)
}

# Recommendation 1653: Krankenversicherung
if (!hasAny(ELTYPE$Krankenversicherung._)) {
  recommendTerms(1653, ELTYPE$Krankenversicherung._)
}

# Recommendation 1652: Haftpflicht
if (!hasAny(ELTYPE$Haftpflichtversicherung._)) {
  recommendTerms(1652, ELTYPE$Krankenversicherung._)  
}

# Recommendation 1655: KFZ-Versicherung
if (!hasAny(ELTYPE$KFZ.Haftpflichtversicherung)) {
  recommend(1655, "TERM", ELTYPE$KFZ.Haftpflichtversicherung)
}

# Recommendation 1656: Lebensversicherung
if (!hasAny(ELTYPE$Lebensversicherung._)) {
  recommendTerms(1656, ELTYPE$Lebensversicherung._)
}

# Recommendation 1654: Hausratsversicherung
if (!hasAny(ELTYPE$Hausratversicherung)) {
  recommend(1654, "TERM", ELTYPE$Hausratversicherung)
}

# Recommendation 1657: BU
if (!hasAny(ELTYPE$Berufsunfähigkeitsversicherung)) {
  recommend(1657, "TERM", ELTYPE$Berufsunfähigkeitsversicherung)
}

#Recommendation 185: Risikolebensversicherung
if (!has(ELTYPE$Risikolebensversicherung)) {
  for (child in getAll(ELTYPE$Kinder._)) {
    if (child$person.geburtsdatum > "1993-01-01") {
      recommend(185, "TERM", ELTYPE$Risikolebensversicherung)
      break;
    }
  }
}

# Recommendation 624: Hauptausgaben
ref <- c(ELTYPE$Miete, 
         ELTYPE$Hausgeld, 
         ELTYPE$Kindertagesbetreuung,
         ELTYPE$Hypothek)
#if (!hasAny(ref)) {
#  for (missing in ref)
#    recommend(624, "TERM", missing)
#}

# Recommendation 625: Einkommensart
#if (!hasAny(ELTYPE$Einkommen._)) {
#  for (missing in ELTYPE$Einkommen._)
#    recommend(625, "TERM", missing)
#}

# Recommendation 897: Kaltmiete eintragen
for (node in getAll(ELTYPE$Mietvertrag)) {
  if (node$miete.betrag.kalt=="") {
    recommend(897, "NODE", node$node_id)
  } 
}
