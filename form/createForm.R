#
# creatForm.R
#
# This script creates the html form that is used for online
# editing of documents.
# This is a static script. Its output must be manually moved to Drupal.
require(shiny, quietly=TRUE)
source("common/getELFIELD.R", encoding="UTF-8")
source("common/getELENUM.R", encoding="UTF-8")


inputFrame <- function(text, inputTag, postfix='') {
  textTag <- tags$div(text, class='form_label_ro')
  if (nchar(postfix)>0)
    tags$section(textTag, inputTag, 
                tags$div(postfix, class='form_label_ro fl_right'))
  else
    tags$section(textTag, inputTag)
}

simpleField <- function(field, text, type='text', postfix='') {
  inputFrame(text,
             tags$input(`data-name`=field, type=type), 
             postfix)
} 

euroField <- function(field, text, step='0.01', postfix='€') {  
  inputFrame(text,
             tags$input(`data-name`=field, type='number', step=step), 
             postfix)
}

textareaField <- function(field, text, rows=5) {
  inputFrame(text,
            tags$textarea(`data-name`=field, rows=rows))
}

selectField <- function(field, text, enum) {  
  values = sapply(enum, function(x) x$value)
  optionNames = sapply(enum, function(x) x$key)
  
  inputFrame(text, 
               tags$select(`data-name`=field,
                  tags$section(lapply(data.frame(rbind(values, optionNames)), 
                                                  function(x) { tags$option(value=x[[1]], x[[2]]) }))))
}
dynamicSelectField <- function(field, text, dataSource) {
  inputFrame(text, 
             tags$select(`data-source`=dataSource, `data-name`=field))
}

frequencyKey <- c('einmalig', 'woche','monat','quartal','halbjahr','jahr')
frequencyName <- c('Einmalig', 'Woche','Monat','Quartal','Halbjahr','Jahr')

form <- tags$div(
simpleField('person.geburtsdatum','Geburtsdatum:','date'),
selectField('person.geschlecht','Geschlecht',ELENUM$person.geschlecht),
textareaField('adresse', 'Adresse:'),
euroField('kauf.wert','Kaufpreis:'),
simpleField('kauf.datum','Kaufdatum:','date'),
simpleField('hersteller','Hersteller:','text'),
simpleField('vertrag.nummer','Vertragsnummer:','text'),
simpleField('vertrag.zahlung.betrag','Vertragliche Zahlung:','number'),
euroField('vertrag.zahlung.betrag.konsum','Vertragliche Zahlung:'),
euroField('vertrag.zahlung.betrag.investition','Vertragliche Zahlung:'),
simpleField('vertrag.zahlung.start','Vertragsbeginn:','date'),
selectField('vertrag.zahlung.frequenz','Zahlungsfrequenz:',ELENUM$vertrag.zahlung.frequenz),
simpleField('vertrag.zahlung.ende','Vertragsende:','date'),
simpleField('mietvertrag.mieter','Dein Mieter:','text'),
textareaField('mietvertrag.adresse', 'Die Adresse der Wohnung:'),
euroField('vermietung.betrag.kalt','Kaltmiete:'),
euroField('vermietung.betrag.nebenkosten','Nebenkosten:'),
simpleField('vermietung.betrag.start','Vermietet seit:','date'),
selectField('vermietung.betrag.frequenz','Zahlungsfrequenz:',ELENUM$vermietung.betrag.frequenz),
simpleField('vermietung.betrag.ende','Befristung:','date'),
euroField('einkommen.betrag.brutto','Bruttoeinkommen:'),
euroField('einkommen.betrag.netto','Nettoeinkommen:'),
selectField('einkommen.betrag.frequenz','Frequenz deines Einkommens:',ELENUM$einkommen.betrag.frequenz),
simpleField('einkommen.betrag.start','Beginn des Einkommens:','date'),
simpleField('einkommen.betrag.ende','Befristung des Einkommens:','date'),
selectField('arbeitsvertrag.beruf','Tätigkeit', ELENUM$arbeitsvertrag.beruf),
simpleField('arbeitsvertrag.arbeitgeber','Arbeitgeber:','text'),
simpleField('arbeitsvertrag.zeitanteil','Beschäftigung:','number',postfix='%'),
simpleField('rente.traeger','Rententräger:','text'),
simpleField('gesetzlicherente.entgeldpunkte.anzahl','Rentenentgeldpunkte:','number'),
simpleField('gesetzlicherente.entgeldpunkte.datum','festgellt am','date'),
euroField('privaterente.grundsumme','Grundsumme:'),
simpleField('privaterente.progression','Rentenprogression:','number'),
simpleField('mietvertrag.vermieter','Vermieter:','text'),
euroField('miete.betrag.kalt','Kaltmiete:'),
euroField('miete.betrag.nebenkosten','Nebenkosten:'),
simpleField('miete.betrag.start','Mietbeginn:','date'),
selectField('miete.betrag.frequenz','Zahlungsfrequenz:',ELENUM$miete.betrag.frequenz),
simpleField('miete.betrag.ende','Mietende:','date'),
euroField('zeitwert.betrag','Zeitwert:'),
simpleField('zeitwert.datum','festgestellt am','date'),
euroField('ratenkredit.restzahlung.betrag','Restzahlung:'),
euroField('kredit.zeitwert.betrag','Zeitwert:'),
simpleField('kredit.zeitwert.datum','festgestellt am','date'),
simpleField('versicherung.tarif','Versicherungstarif:','text'),
euroField('versicherung.deckungssumme.betrag','Deckungssumme:'),
euroField('versicherung.selbstbeteiligung','Selbstbeteiligung:'),
simpleField('versicherung.ausfalldeckung','Ausfalldeckung:','checkbox'),
euroField('versicherung.haftpflicht.schluesselverlust','Schlüsselverlust:'),
euroField('invaliditaet.rente.betrag','Versicherter Betrag:'),
simpleField('invaliditaet.rente.bezugsende','Bezugsende:','number'),
simpleField('bu.versicherterberuf','Versicherter Beruf','text'),
simpleField('bankkonto.iban','IBAN:','text'),
dynamicSelectField('kfzversicherung.fahrzeug.ref','Versichertes Fahrzeug:','Fahrzeug'),
euroField('versicherung.todesfallleistung','Todesfallleistung:'),
euroField('versicherung.unfallrente','Unfallrente:'),
simpleField('schadensabdeckung.privat','Private Schadensabdeckung:','checkbox'),
simpleField('schadensabdeckung.beruf','Berufliche Schadensabdeckung:','checkbox'),
simpleField('schadensabdeckung.verkehr','Schadensabdeckung Verkehr:','checkbox'),
simpleField('schadensabdeckung.wohnen','Schadensabdeckung Wohnen:','checkbox'),
simpleField('schadensabdeckung.vermietung','Schadensabdeckung Vermietung:','checkbox'),
simpleField('schadensabdeckung.blitzschlag','Schadensabdeckung Blitzschlag:','checkbox'),
simpleField('schadensabdeckung.sturm','Schadensabdeckung Sturm:','checkbox'),
simpleField('schadensabdeckung.hagel','Schadensabdeckung Hagel:','checkbox'),
simpleField('schadensabdeckung.leitungswasser','Schadensabdeckung Leitungswasser:','checkbox'),
simpleField('schadensabdeckung.elementar','Schadensabdeckung Elementar:','checkbox'),
simpleField('auto.kennzeichen','Autokennzeichen:','text'),
simpleField('auto.schluesselnummer','Schlüsselnummer:','text'),
simpleField('auto.fahrzeugidentnummer','Fahrzeug-Identifizierungsnummer (FIN):','text'),
simpleField('boot.klasse','Bootsklasse:','text'),
simpleField('motorrad.klasse','Motorradklasse:','text'),
simpleField('fahrrad.rahmennummer','Fahrradrahmennummer:','text'),
simpleField('immobilie.wohnflaeche','Wohnfläche:','number',postfix='qm'),
simpleField('immobilie.grundstuecksflaeche','Grundstücksfläche:','number',postfix='qm'),
simpleField('immobilie.grundbuchnummer','Grundbuchnummer:','text'),
simpleField('pferd.equidenpass','Equidenpass:','text'),
simpleField('hund.hundemarke','Hundemarke:','text'),
simpleField('bausparen.tarif','Bauspartarif:','text'),
euroField('bausparen.summe','Bausparsumme:'),
euroField('bausparen.mindestsparguthaben','Bausparmindestguthaben:'),
textareaField('kommentarfeld','Kommentar', rows=3)
)

write(as.character(form), stdout())
