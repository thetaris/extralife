#get current working directory
wd <- getwd()
#file name
fileName <- "data/DEU_1D_InsMort_prob_ERGO_Zielbild_Leben.dat"
readName <- paste(wd, fileName, sep = "/")
#read data file
dataSet <- read.csv(readName, sep = "\t")
n = nrow(dataSet)
#barplot bar color set
colSet <- function(sex){
  if (sex == 1) "red"
  else{ 
    if (sex == 2) "goldenrod1"
    else "lightgreen"
  }
}
#barplot bar border color set
barBorderColor <- function(sex){
  if (sex == 1) "red2"
  else{
    if(sex == 2) "darkgoldenrod2"
    else "green2"
  }
}

fillZeros <- function(age){
  if (age < 1)
    fillZeros <- c(rep(0, age))
  else
    fillZeros <- c(rep(0, age-1))
}

findAgeIndex <- function(survivalProb, probs){
  xInd = NULL
  for (j in 1:length(probs)){
    xInd[j] <- which.min(abs(survivalProb-probs[j]))
  }
  return(xInd)
}
  
#1st col data is for female, 2nd col data is for male
# 'sex' assumes values of 1 or 2, 1 for female, 2 for male
barPlotMort <- function(birthYear, sex, name) {
  
  if (birthYear==0) {
    emptyData <- c(rep(0,n))
    names(emptyData) <- c(seq(2013,2013+n-1))
    barplot(emptyData, xaxs ="i",yaxs ="i",ylim=c(0,1),
            col = "white", xaxt="n", yaxt = "n",
            xlim=c(0,length(emptyData)-1),
            tcl = -0.2, cex.axis = 0.8, 
            xlab="",ylab="", border="white")
    title(main = name, col.main = "grey48", font.main = 4)
    text(60,0.5,"Birth year data not available", col = "grey48", cex = 1, font = 4)
    box(col="dodgerblue")
    yvec = seq(0,1, by = 0.2)
    names(yvec) <- c("0","20%","40%","60%","80%","100%")
    axis(2,  at = yvec, labels=names(yvec), las = 1, cex.axis = 0.7,
         yaxs ="i", tcl = -0.2, col.ticks=4, hadj = 0.5)
    
    return()
  }
  else{
    if(birthYear == 2013)
      age = 2013 - birthYear +1
    else
      age = 2013 - birthYear 
  }
    
  #if sex=0, use dataNA
  dataNA <- (dataSet[,1]+ dataSet[,2])/2
  dataAll <- cbind(dataSet,dataNA)
  #cumulative survival probilities
  if(sex==0)
    data = cumprod(1-dataAll[age:n,3])
  else
    data = cumprod(1-dataAll[age:n,sex])
  
  plotData = c(data, fillZeros(age))
  
  xIndex <- findAgeIndex(data,c(0.05,0.5,0.95))
    
  
  barplot(plotData, xaxt="n", yaxt = "n", ylim=c(0,1),
          xlab="", ylab="", 
       border = barBorderColor(sex), space = 0.1, width = 0.9,
       tcl = -0.2, cex.axis = 0.8,  xlim=c(0,n-1),
       yaxs ="i",  xaxs ="i",
       col = colSet(sex)) 
  
  title(main = name, col.main = barBorderColor(sex), font.main = 4)
  box(col="dodgerblue")
  lines(x=seq(0,n-1),plotData, yaxs ="i", xaxs ="i",xlim=c(0,n-1), xaxt="n",
      col = sex, lty = "dashed", lwd = 2)

 
  vlinePos <- c(xIndex)
  
  abline(v=vlinePos, col = "slateblue3", lwd=1)
  
  text(vlinePos,c(0.3,0.3,0.3),c("5%","50%","95%"), 
       col = c("black","black","black"), cex=0.75)
  text(vlinePos,c(0.1,0.1,0.1),c(xIndex+2013), 
       col = c("black","black","black"), cex=0.75)
  yvec = seq(0,1, by = 0.2)
  names(yvec) <- c("0","20%","40%","60%","80%","100%")
  axis(2,  at = yvec, labels=names(yvec), las = 1, cex.axis = 0.7,
       yaxs ="i", tcl = -0.2, col.ticks=4, hadj = 0.5)
  textLabels <- paste("50% prob. survive in", xIndex[2]+2013) 
  text(n-nchar(textLabels), 0.8,
         labels=textLabels,
         cex=0.8)
  
}

plotFamilyMort <- function(dataid) {
  nr = nrow(dataid)
 
  layout(matrix(1:nr, ncol=1))
  names <- dataid$name
  birthYears <- dataid$birthYear
  sexes <- dataid$sex
  for (i in 1:nr) {
     par(mar = c(1.8,2,1.5,1))
      barPlotMort(birthYears[i], sexes[i], names[i])
  }
  
  par(new=TRUE,lab = c(24,5,10))
  xvec <- seq(0,n-1,10)
  names(xvec)<- seq(2013, 2013+n-1,10)
  axis(1,  at = xvec, labels=names(xvec), cex.axis = 0.8,
       xaxs ="i",tcl = -0.2, col.ticks=4, padj = -1.5)

}



