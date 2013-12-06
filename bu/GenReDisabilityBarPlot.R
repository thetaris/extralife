  
#get current working directory
wd <- getwd()
#file name
fileName1 <- "GenRe_man.csv"
fileName2 <- "GenRe_woman.csv"

#read data file
#male disability data
dataSet1 <- read.csv(fileName1, sep = ",")
dfDataSet1 <- data.frame(dataSet1)
#female disability data
dataSet2 <- read.csv(fileName2, sep = ",")
dfDataSet2 <- data.frame(dataSet2)
#cumulative probabilities
cumProb <- function(prob,age){
  cumP = NULL
  cumP[age-15+1]=prob[age-15+1]
  for (i in age-15+2:length(prob)){
    cumP[i]=cumP[i-1]+(1-cumP[i-1])*prob[i]
  }
  return (cumP)
}

#bar plot of male and female cumulative disability probabilities
DisabilityBarGraph <- function(age, gender, group){
 library(car)
  ageData <-dfDataSet1$X
  
  dfDataSet1$Gruppe.1<-as.numeric(as.vector(recode(dfDataSet1$Gruppe.1,"'-'=0.0")))
  maleG1 <- cumProb(dfDataSet1$Gruppe.1/1000*1/1.45,age)
  maleG2 <- cumProb(dfDataSet1$Gruppe.2/1000*1/1.45,age)
  maleG3 <- cumProb(dfDataSet1$Gruppe.3/1000*1/1.4,age)
  maleG4 <- cumProb(dfDataSet1$Gruppe.4/1000*1/1.4,age)
  maleDisabilityData <- cbind(maleG1,maleG2,maleG3,maleG4)
  
  dfDataSet2$Gruppe.1<-as.numeric(as.vector(recode(dfDataSet2$Gruppe.1,"'-'=0.0")))
  femaleG1 <- cumProb(dfDataSet2$Gruppe.1/1000*1/1.45,age)
  femaleG2 <- cumProb(dfDataSet2$Gruppe.2/1000*1/1.45,age)
  femaleG3 <- cumProb(dfDataSet2$Gruppe.3/1000*1/1.42,age)
  femaleG4 <- cumProb(dfDataSet2$Gruppe.4/1000*1/1.42,age)
  femaleDisabilityData <- cbind(femaleG1,femaleG2,femaleG3,femaleG4)
  
  mainTitle1 <- "Male Disability Probilities By Age"
  mainTitle2 <- "Female Disability Probilities By Age"

  yAxisLimit <- c(0, 1)
  yAxisTicks <- seq(0, 1, by=0.25)
  yAxisLabel <- c("0%", "25%", "50%", "75%", "100%")
  #bar colors 
  colorSet <- function(gender){
    if(gender=="f") "lightgreen"
    else{
      if(gender=="m") "deepskyblue" 
    }
  }
  #bar border colors  
  borderColorSet <- function(gender){
      if(gender=="f") "green2"
      else{
        if(gender=="m") "darkslategray2"
      }
    }

  #male bar plot
  if(gender=="m"){
        xAxisLimitm <- c(age-15+1, length(ageData))  
        
        barplot(maleDisabilityData[,group][age-15+1:length(ageData)-(age-15)],
          xlab = "", space=0.11,width=0.9,
          ylab = "", xaxt="n",yaxt ="n", beside=T,
          xlim = xAxisLimitm, ylim = yAxisLimit, col =colorSet(gender),
          border=borderColorSet(gender)
        )
      lines(maleDisabilityData[,group][age-15+1:length(ageData)-(age-15)],
            type="l", col="yellow",lwd=1.5)
      title(main = mainTitle1, col.main = "blue4", 
          cex.main = 0.8, font.main=4)
        
      yProbsm <- round(c(maleDisabilityData[,group][50-15+1],
                          maleDisabilityData[,group][60-15+1],
                          maleDisabilityData[,group][64-15+1])*100, 
                          digits=0)
      vlinePos <- c(50,60,64)-15+1
        
      abline(v=vlinePos, col = "slateblue3", lwd=1)
        
      text(c(vlinePos[1:2]-2.8,vlinePos[3]-2),c(0.38,0.6,0.8),
           c(paste(yProbsm[1], "% \n probability \n of disability \n at age 50"),
             paste(yProbsm[2], "% \n probability \n of disability \n at age 60"),
             paste(yProbsm[3], "% \n probability \n of disability \n at age 64")), 
             col = c("black","black","black"), cex=0.5)
     
      xAxisTicksm <- c(seq(age-15+1,length(ageData), by=5), length(ageData))
      xAxisLabelm <- c(seq(ageData[age-15+1],ageData[length(ageData)], by=5), 
                ageData[length(ageData)])
      axis(side = 1, at = xAxisTicksm, 
          labels = xAxisLabelm, 
          xaxs="i", tcl = -0.2, padj = -2,
          col.ticks=4, cex.axis = 0.65, lwd=0.5)

      axis(2, at = yAxisTicks, 
          labels = yAxisLabel,
          las =1, lwd = 0.5,
          yaxs = "r", tcl = -0.2, col.ticks=4, cex.axis = 0.65,
          hadj = 0.5)
    }
else{ #female bar plot
  if(gender=="f"){
    #female disability probabilities
    xAxisLimitf <- c(age-15+1, length(ageData))
    barplot(femaleDisabilityData[,group][age-15+1:length(ageData)-(age-15)], 
            xlab = "", space=0.11, width=0.9,
            ylab = "", xaxt ="n",yaxt ="n", beside=T,
            xlim = xAxisLimitf, ylim = yAxisLimit, col =colorSet(gender),
            border=borderColorSet(gender)
        )
    lines(femaleDisabilityData[,group][age-15+1:length(ageData)-(age-15)],
          type="l", col="hotpink",lwd=1.5)
    
    yProbsf <- round(c(femaleDisabilityData[,group][50-15+1],
                femaleDisabilityData[,group][60-15+1],
                femaleDisabilityData[,group][64-15+1])*100, 
                digits=0)
    vlinePos <- c(50,60,64)-15+1
    
    abline(v=vlinePos, col = "forestgreen", lwd=1)
    
    text(c(vlinePos[1:2]-2.8,vlinePos[3]-2),c(0.3,0.5,0.8),
         c(paste(yProbsf[1], "% \n probability \n of disability \n at age 50"),
           paste(yProbsf[2], "% \n probability \n of disability \n at age 60"),
           paste(yProbsf[3], "% \n probability \n of disability \n at age 64")), 
         col = c("black","black","black"), cex=0.5)
    
    title(main = mainTitle2, col.main = "blue4", 
          cex.main = 0.8, font.main = 4)
    xAxisTicksf <- c(seq(age-15+1,length(ageData), by=5), length(ageData))
    xAxisLabelf <- c(seq(ageData[age-15+1],ageData[length(ageData)], by=5), 
                    ageData[length(ageData)])
    axis(side = 1, at = xAxisTicksf, 
        labels = xAxisLabelf, 
        xaxs="i", tcl = -0.2, padj = -2,
        col.ticks=4, cex.axis = 0.65, lwd=0.5)

    axis(2, at = yAxisTicks, 
        labels = yAxisLabel,
        las =1, lwd = 0.5,
        yaxs = "r", tcl = -0.2, col.ticks=4, cex.axis = 0.65,
        hadj = 0.5)
    }
  }
}

PlotDisabilityBarGraph<-function(age,gender,profession_group){
  layout(matrix(length(gender):1, ncol=1))
  
  par(mar = c(2,2.5,1.5,1))
  
  for (i in 1:length(gender)){
    DisabilityBarGraph(age[i],gender[i],profession_group[i])  
  }
  
}


age = c(35, 20)
gender = c("m", "f")
profession_group = c(3,2)

PlotDisabilityBarGraph(age,gender,profession_group)
