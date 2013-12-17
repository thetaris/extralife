testCaller <- function(){
  dataObj <- DGSData(file = "../test/data/test_simpson.json" )
  tmp = dataObj$get("person.geschlecht")
  tmp = dataObj$get("person.geburtsdatum")
  tmp = dataObj$get("person.geburtsdatum", type=ELTYPE$Meine_Familie._)
  tmp = dataObj$get("person.geburtsdatum", type=ELTYPE$Ich)
  print(dataObj$getLog())
}
