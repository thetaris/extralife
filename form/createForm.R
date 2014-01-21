#
# creatForm.R
#
# This script creates the html form that is used for online
# editing of documents.
# This is a static script. Its output must be manually moved to Drupal.
require(shiny, quietly=TRUE)

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

numberField <- function(field, text, type='text', postfix='', step='0.01') {
  inputFrame(text,
             tags$input(`data-name`=field, type=type, step=step), 
             postfix)
}

textareaField <- function(field, text, rows=5) {
  inputFrame(text,
            tags$textarea(`data-name`=field, rows=rows))
}

selectField <- function(field, text, values, optionNames) {
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
selectField('person.geschlecht','Geschlecht',c('na','mann','frau'),c('--','männlich','weiblich')),
textareaField('adresse', 'Adresse:'),
numberField('kauf.wert','Kaufpreis:','number',postfix='€'),
simpleField('kauf.datum','Kaufdatum:','date'),
simpleField('hersteller','Hersteller:','text'),
simpleField('vertrag.nummer','Vertragsnummer:','text'),
simpleField('vertrag.zahlung.betrag','Vertragliche Zahlung:','number'),
simpleField('vertrag.zahlung.betrag.konsum','Vertragliche Zahlung:','number',postfix='(Konsum)'),
simpleField('vertrag.zahlung.betrag.investition','Vertragliche Zahlung:','number',postfix='(Investition)'),
simpleField('vertrag.zahlung.start','Vertragsbeginn:','date'),
selectField('vertrag.zahlung.frequenz','Zahlungsfrequenz:',frequencyKey, frequencyName),
simpleField('verttrag.zahlung.ende','Vertragsende:','date'),
simpleField('mietvertrag.mieter','Dein Mieter:','text'),
textareaField('mietvertrag.adresse', 'Die Adresse der Wohnung:'),
simpleField('vermietung.betrag.kalt','Kaltmiete:','number'),
simpleField('vermietung.betrag.nebenkosten','Nebenkosten:','number'),
simpleField('vermietung.betrag.start','Vermietet seit:','date'),
selectField('vermietung.betrag.frequenz','Zahlungsfrequenz:',frequencyKey, frequencyName),
simpleField('vermietung.betrag.ende','Befristung:','date'),
simpleField('einkommen.betrag.brutto','Bruttoeinkommen:','number'),
simpleField('einkommen.betrag.netto','Nettoeinkommen:','number'),
selectField('einkommen.betrag.frequenz','Frequenz deines Einkommens:',frequencyKey, frequencyName),
simpleField('einkommen.betrag.start','Beginn des Einkommens:','date'),
simpleField('einkommen.betrag.ende','Befristung des Einkommens:','date'),
selectField('arbeitsvertrag.beruf','Tätigkeit',c('na','1','2', '3', '4'),c('--','akademische Tätigkeit','einfachere Bürotätigkeit', 'leichte körperliche Arbeit', 'schwere körperliche Arbeit'))
simpleField('arbeitsvertrag.arbeitgeber','Arbeitgeber:','text'),
simpleField('arbeitsvertrag.zeitanteil','Beschäftigung:','number',postfix='%'),
simpleField('rente.traeger','Rententräger:','text'),
simpleField('gesetzlicherente.entgeldpunkte.anzahl','Rentenentgeldpunkte:','number'),
simpleField('gesetzlicherente.entgeldpunkte.datum','festgellt am','date'),
simpleField('privaterente.grundsumme','Grundsumme:','number'),
simpleField('privaterente.progression','Rentenprogression:','number'),
simpleField('mietvertrag.vermieter','Vermieter:','text'),
simpleField('miete.betrag.kalt','Kaltmiete:','number'),
simpleField('miete.betrag.nebenkosten','Nebenkosten:','number'),
simpleField('miete.betrag.start','Mietbeginn:','date'),
selectField('miete.betrag.frequenz','Zahlungsfrequenz:',frequencyKey, frequencyName),
simpleField('miete.betrag.ende','Mietende:','date'),
numberField('zeitwert.betrag','Zeitwert:','number',postfix='€'),
simpleField('zeitwert.datum','festgestellt am','date'),
numberField('kredit.zeitwert.betrag','Zeitwert:','number',postfix='€'),
simpleField('kredit.zeitwert.datum','festgestellt am','date'),
simpleField('versicherung.tarif','Versicherungstarif:','text'),
simpleField('versicherung.deckungssumme.betrag','Deckungssumme:','number'),
simpleField('versicherung.selbstbeteiligung','Selbstbeteiligung:','number'),
simpleField('versicherung.ausfalldeckung','Ausfalldeckung:','checkbox'),
simpleField('versicherung.haftpflicht.schluesselverlust','Schlüsselverlust:','number'),
simpleField('invaliditaet.rente.betrag','Versicherter Betrag:','number'),
simpleField('invaliditaet.rente.bezugsende','Bezugsende:','number'),
simpleField('bu.versicherterberuf','Versicherter Beruf','text'),
simpleField('bankkonto.iban','IBAN:','text'),
dynamicSelectField('kfzversicherung.fahrzeug.ref','Versichertes Fahrzeug:','Fahrzeug'),
simpleField('versicherung.todesfallleistung','Todesfallleistung:','number'),
simpleField('versicherung.unfallrente','Unfallrente:','number'),
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
simpleField('auto.fahrzeugidentnummer','Fahrzeugidentnummer:','text'),
simpleField('boot.klasse','Bootsklasse:','text'),
simpleField('motorrad.klasse','Motorradklasse:','text'),
simpleField('fahrrad.rahmennummer','Fahrradrahmennummer:','text'),
simpleField('immobilie.wohnflaeche','Wohnfläche:','number',postfix='qm'),
simpleField('immobilie.grundstuecksflaeche','Grundstücksfläche:','number',postfix='qm'),
simpleField('immobilie.grundbuchnummer','Grundbuchnummer:','text'),
simpleField('pferd.equidenpass','Equidenpass:','text'),
simpleField('hund.hundemarke','Hundemarke:','text'),
simpleField('bausparen.tarif','Bauspartarif:','text'),
simpleField('bausparen.summe','Bausparsumme:','text'),
simpleField('bausparen.mindestsparguthaben','Bausparmindestguthaben:','text'),
textareaField('kommentarfeld','Kommentar', rows=3)
)

write(as.character(form), stdout())
