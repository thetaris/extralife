#Balance App
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

# Recommendation 624: Hauptausgaben
ref <- c(ELTYPE$Miete, 
         ELTYPE$Hausgeld, 
         ELTYPE$Kindertagesbetreuung,
         ELTYPE$Hypothek)
recommend("Bestimme Deine Hauptausgaben", hasAny(ref), term=ref)

# Recommendation 625: Einkommensart
recommend("Bestimme Deine Einkommensart", hasAny(ELTYPE$Einkommen._), term=(ELTYPE$Einkommen._))

# Recommendation 897: Kaltmiete eintragen
for (node in getAll(ELTYPE$Mietvertrag)) {
  if (node$miete.betrag.kalt=="") {
    recommend("Kaltmiete eintraget", node=node$node_id)
  } 
}

# Data access
fields <-list()

relevantTypes <- c(ELTYPE$Mein.Besitz._, ELTYPE$Meine.Versicherungen._, ELTYPE$Meine.Vertraege._)

# generate the following template
#
# cmdStr <- ""
# for (iterField in names(ELFIELD)){
#   cmdStr<-(sprintf("%sfields[[ELFIELD$%s]] <- relevantTypes \n", cmdStr, iterField))
# }
# cat(cmdStr)

fields[[ELFIELD$title]] <- relevantTypes 
fields[[ELFIELD$kauf.datum]] <- relevantTypes 
fields[[ELFIELD$kauf.wert]] <- relevantTypes 
fields[[ELFIELD$vertrag.zahlung.betrag]] <- relevantTypes 
fields[[ELFIELD$vertrag.zahlung.betrag.konsum]] <- relevantTypes 
fields[[ELFIELD$vertrag.zahlung.betrag.investition]] <- relevantTypes 
fields[[ELFIELD$vertrag.zahlung.start]] <- relevantTypes 
fields[[ELFIELD$vertrag.zahlung.frequenz]] <- relevantTypes 
fields[[ELFIELD$vertrag.zahlung.ende]] <- relevantTypes 
fields[[ELFIELD$vermietung.betrag.kalt]] <- relevantTypes 
fields[[ELFIELD$vermietung.betrag.nebenkosten]] <- relevantTypes 
fields[[ELFIELD$vermietung.betrag.start]] <- relevantTypes 
fields[[ELFIELD$vermietung.betrag.frequenz]] <- relevantTypes 
fields[[ELFIELD$vermietung.betrag.ende]] <- relevantTypes 
fields[[ELFIELD$einkommen.betrag.brutto]] <- relevantTypes 
fields[[ELFIELD$einkommen.betrag.netto]] <- relevantTypes 
fields[[ELFIELD$einkommen.betrag.frequenz]] <- relevantTypes 
fields[[ELFIELD$einkommen.betrag.start]] <- relevantTypes 
fields[[ELFIELD$einkommen.betrag.ende]] <- relevantTypes 
fields[[ELFIELD$miete.betrag.kalt]] <- relevantTypes 
fields[[ELFIELD$miete.betrag.nebenkosten]] <- relevantTypes 
fields[[ELFIELD$miete.betrag.start]] <- relevantTypes 
fields[[ELFIELD$miete.betrag.frequenz]] <- relevantTypes 
fields[[ELFIELD$miete.betrag.ende]] <- relevantTypes 
fields[[ELFIELD$zeitwert.betrag]] <- relevantTypes 
fields[[ELFIELD$zeitwert.datum]] <- relevantTypes 
fields[[ELFIELD$ratenkredit.restzahlung.betrag]] <- relevantTypes 
fields[[ELFIELD$kredit.zeitwert.betrag]] <- relevantTypes 
fields[[ELFIELD$kredit.zeitwert.datum]] <- relevantTypes 

result$dataaccess <- list(types = relevantTypes, fields = fields)

cat(toJSON(result))