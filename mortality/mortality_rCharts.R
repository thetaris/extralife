getDensitiesFromFile <- function(fileName = "data/DEU_1D_InsMort_prob_ERGO_Zielbild_Leben.dat"){
  #get current working directory
  wd <- getwd()
  readName <- paste(wd, fileName, sep = "/")
  #read data file
  dataSet <- read.csv(readName, sep = "\t", header = FALSE)
}

getProbabilities <- function(age_in, sex){
  # sex : 1<- male, 2<- female, 0<- unknown
  densities <- getDensitiesFromFile()
  n <- nrow(densities)
  age <- round(age_in)
    
  if (sex==0){
    result <- ( cumprod(1-densities[age:n,1]) + cumprod(1-densities[age:n,2]) )/2 
  } else {
    if (sex=="mann"){
      result <- cumprod(1-densities[age:n,1]) 
    }else if (sex=="frau"){
      result <- cumprod(1-densities[age:n,2]) 
    }else{
      stop(sprintf("Unbekanntes Geschlecht (sex=%s)", sex))
    }
  }
  return(result)
}

findAgeIndex <- function(survivalProb, probs){
  xInd = NULL
  for (j in 1:length(probs)){
    xInd[j] <- which.min(abs(survivalProb-probs[j]))
  }
  return(xInd)
}

findSurvivalQuantile<-function(birthDay, sex, quantile){
  age <- as.numeric(Sys.Date()-birthDay)/365.24
  plotProb <- getProbabilities(age, sex)
  yearsToGo <- findAgeIndex(plotProb, quantile)
  result <- list()
  result$age <- age + yearsToGo
  result$year <- as.numeric(format(Sys.Date(),"%Y"))+ yearsToGo
  return(result)
}

splitDataByAge<-function(data, age){
  # initialize variables  
  res<-list()
  res$education = data
  res$working = data
  res$retirement = data
  if (length(age)==1){
    age = age:(age+length(data)-1)
  }
  # set to zero where age is not in phase
  res$education[age>=24] = 0
  res$working[(age<24)|(age>=67)] = 0
  res$retirement[(age<67)] = 0
  return(res)
}

mortalityHistogram <- function(birthDay, sex, name){
  if (length(name)==0){
    return(NULL)
  }
  
  age <- as.numeric(Sys.Date()-birthDay)/365.24
  plotProb <- getProbabilities(age, sex)
  
  currentYear = as.numeric(format(Sys.Date(), "%Y"))
  
  plotYear <- currentYear:(currentYear+length(plotProb)-1)
  
  splittedData <- splitDataByAge(plotProb, age)    
  
  plotDataEducation <- data.frame(plotYear, phase = "Ausbildung", prob=splittedData$education)
  plotDataWorking <- data.frame(plotYear, phase = "Arbeit", prob=splittedData$working)
  plotDataRetirement <- data.frame(plotYear, phase = "Ruhestand", prob=splittedData$retirement)    
  
  plotData<-rbind(plotDataEducation, plotDataWorking, plotDataRetirement)
  
  hist<-nPlot(prob~plotYear, data = plotData, type = "multiBarChart", group = "phase"  )
  hist$chart(reduceXTicks = "true")
  #hist$xAxis(staggerLabels = FALSE, showMaxMin=FALSE)
  hist$chart(stacked = "true")
  
  birthYear = as.numeric(format(birthDay, "%Y"))  

  hist$chart(tooltip = sprintf("#!function(key, x, y, e, graph){ return '<h3> Alter '+ (x - %i) + '</h3>' +
                '<p>' + x + '</p>' +'<p>' + key + '</p>' + '<p>' + y + '</p>' ;}!#", birthYear))
  
  hist$yAxis(tickFormat = "#!function(d) {return (100*d).toFixed(0) + '%';}!#")
  
#   n1$chart(tooltip = "#!function(key, x, y, e, graph){ return '<h3>'+ x + '</h3>' +
#                 '<p>' + key + '</p>' + '<p>' + ((d3.time.format('%Y').parse(y).getTime()-(new Date()).getTime())/1000/60/60/24/365.24).toFixed(0)  
#           + ' Jahre</p>' ;}!#")
#   
  #n1$chart(width = 700)

  
#   n1$yAxis(tickFormat = "#!function(d) {return d3.time.format('%Y')( new Date( (new Date()).getTime() + d * 86400000*365.24 ));}!#")
#   n1$yAxis(axisLabel = "Zeit in Jahren")
  
  return(hist)
}



