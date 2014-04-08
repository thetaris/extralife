source('common/INIT.R', chdir=TRUE, encoding="UTF-8")

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


data <- list(types = relevantTypes,
             fields = fields)

write(toJSON(data),'balance/dataaccess.json')
