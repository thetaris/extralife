testCaller <- function(){
  dataObj <- DGSData(file = "../test/testdata.json" )
  tmp = dataObj$get("person.geschlecht")
  tmp = dataObj$get("person.geburtsdatum")
  tmp = dataObj$get("person.geburtsdatum", getType_idFromTaxonomyMap()$Meine_Familie)
  tmp = dataObj$get("person.geburtsdatum", getType_idFromTaxonomyMap()$ich)
  print(dataObj$getLog())
}