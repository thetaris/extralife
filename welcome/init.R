source('common/INIT.R', chdir=TRUE, encoding="UTF-8")

fields <-list()
fields[[ELFIELD$person.geschlecht]] = ELTYPE$Ich
fields[[ELFIELD$person.geburtsdatum]] = ELTYPE$Ich

data <- list(types = ELTYPE$Ich,
             fields = fields)

write(toJSON(data),'welcome/dataaccess.json')
