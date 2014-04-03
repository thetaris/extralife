source('common/INIT.R', chdir=TRUE, encoding="UTF-8")

fields <-list()
fields[[ELFIELD$person.geschlecht]] = ELTYPE$Ich
fields[[ELFIELD$person.geburtsdatum]] = ELTYPE$Meine.Familie._

data <- list(types = ELTYPE$Meine.Familie._,
             fields = fields)

write(toJSON(data),'mortality/dataaccess.json')
