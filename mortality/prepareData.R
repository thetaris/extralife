prepareDataFamily<-function(dataObj){
  name <- dataObj$get("title", type=ELTYPE$Meine.Familie._)
  birthDay <- dataObj$get("person.geburtsdatum", type=ELTYPE$Meine.Familie._)
  sex <- dataObj$get("person.geschlecht", type=ELTYPE$Meine.Familie._)
  
  # convert data to fit other algorithms
  birthDay[birthDay==""] = NA
  birthYear = sapply(birthDay, function(x) as.numeric(format(as.Date(x), "%Y")))
  birthYear[is.na(birthYear)] = 0
  
  sex[sex==""]    <- 0
  sex[is.na(sex)] <- 0
  
  data = data.frame(name, birthYear, birthDay, sex, row.names=NULL, stringsAsFactors=FALSE)
  
  # sort by age
  data = data[order(-data$birthYear),]
}

prepareDataDemography<-function(){
  PopulationForecastDE<<-read.delim(file = "../mortality/data/PopulationForecastDE.txt", header = TRUE, )  
}

getDensitiesFromFile <- function(fileName = "../mortality/data/DEU_1D_InsMort_prob_ERGO_Zielbild_Leben.dat"){
  #get current working directory
  wd <- getwd()
  readName <- paste(wd, fileName, sep = "/")
  #read data file
  dataSet <- read.csv(readName, sep = "\t", header = FALSE)
}