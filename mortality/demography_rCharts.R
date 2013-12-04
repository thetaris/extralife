demographyChart <- function(currentYear, name, birthYear, gender_in){
  # Creates the demography charts of Germany including persons
  # 
  # example:
  # 
#   currentYear = 2013
#   name = c("Homer", "Marge")
#   birthYear = c(1987, 1983)
#   gender_in = c('m', 'w')
  # pf = demographyChart(currentYear, name, birthYear, gender_in)
  # pf
  
  # make age/gender group unique such that there is only one entry per age/gender group
  if (length(name>1)){
    uniqueName = c(name[1])
    uniqueAge = c( ((currentYear - birthYear[1]) %/% 5)*5 + (gender_in[1]=="w") )
    uniqueBirthYear = c(birthYear[1])
    uniqueGender_in = c(gender_in[1])
    for (iterPers in 2:length(name)){
      newAge = ((currentYear - birthYear[iterPers]) %/% 5)*5 + (gender_in[iterPers]=="w")
      if (sum(uniqueAge == newAge)>0){
        uniqueName[uniqueAge == newAge] = sprintf("%s, %s", uniqueName[uniqueAge == newAge], name[iterPers]) 
      } else {
        uniqueName = c(uniqueName, name[iterPers])
        uniqueAge = c(uniqueAge, newAge)
        uniqueBirthYear = c(uniqueBirthYear, birthYear[iterPers])
        uniqueGender_in = c(uniqueGender_in, gender_in[iterPers])
      }
      
    }
    name = uniqueName
    birthYear = uniqueBirthYear
    gender_in = uniqueGender_in
  }
  
  #PopulationForecastDE<<-read.delim(file = "../mortality/data/PopulationForecastDE.txt", header = TRUE, )
  
  # select subset of data with correct year
  data = PopulationForecastDE[PopulationForecastDE$X==currentYear,]
  
  # present male data to the left using negative values
  data[data$X.1=="m",3:22]=-as.numeric(data[data$X.1=="m",3:22])
  data[data$X.1=="w",3:22]= as.numeric(data[data$X.1=="w",3:22])
  
  # rearrange data for plotting 
  group = c()
  forecast = c()
  age = c()
  gender = c()
  for (iterCols in 3:22){  
    for (iterGender in c("m", "w")){
      group    = c(group, sprintf("%i - %i", (iterCols-3)*5, (iterCols-2)*5))
      forecast = c(forecast, data[data$X.1==iterGender, iterCols]) 
      age      = c(age, (iterCols-2)*5)
      gender   = c(gender, iterGender)
    }
  }
  splitted = splitDataByAge(data=forecast, age)
  plotDataEducation = data.frame(age, group, forecast=splitted$education, phase="Ausbildung", gender, stringsAsFactors = FALSE)
  plotDataWorking = data.frame(age, group, forecast=splitted$working, phase="Arbeit", gender, stringsAsFactors = FALSE)
  plotDataRetired = data.frame(age, group, forecast=splitted$retirement, phase="Ruhestand", gender, stringsAsFactors = FALSE)
  
  plotData = rbind(plotDataEducation, plotDataWorking, plotDataRetired)
  
  # include persons and remove corresponding population bar
  for (iterPersons in 1:length(name)){
    personData = data.frame(age, group, forecast=forecast, phase="to come", gender, stringsAsFactors = FALSE)
    
    ageTmp = currentYear - birthYear[iterPersons]
    genderTmp = gender_in[iterPersons]
    
    sel = ((personData$age<=ageTmp) | (personData$age>(ageTmp+5))) | 
      !(personData$gender==genderTmp)
    personData[sel, "forecast"] = 0
    plotData[!sel,"forecast"] = 0
    personData[,"phase"] = name[iterPersons]
    plotData = rbind(plotData, personData)
  }
  plotData = plotData[order(-plotData$age),]
  pf<-nPlot(forecast~age, group="phase", data = plotData, type = "multiBarHorizontalChart")
  pf$chart(stacked = T, showControls = F)
  
  pf$yAxis(tickFormat="#!function(d) {if (d>0) {res = d/1000 + ' Mio. Frauen'} else {res = -d/1000 + ' Mio. Männer'} return res;}!#" )
  pf$yAxis(showMaxMin = F)
  return(pf)
}