#get current working directory
wd <- getwd()
#file name
fileName <- "data/DEU_1D_InsMort_prob_ERGO_Zielbild_Leben.dat"
readName <- paste(wd, fileName, sep = "/")
#read data file
dataSet <- read.csv(readName, sep = "\t")
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
  if (sex == 1) "red3"
  else{
    if(sex == 2) "darkgoldenrod3"
    else "green4"
  }
}
#barplot dashed line color set
lineColSet <- function(sex){
  if (sex == 1) "olivedrab2"
  else {
    if(sex == 2) "blue"
    else "hotpink2"
  }
}
fillZeros <- function(age){
  if (age < 1)
    fillZeros <- c(rep(0, age))
  else
    fillZeros <- c(rep(0, age-1))
}
#1st col data is for female, 2nd col data is for male
# 'sex' assumes values of 1 or 2, 1 for female, 2 for male
barPlotMort <- function(birthYear, sex, name) {
  if (birthYear==0) {
    plot(seq(0,1,step=0.1))
    return()
  }
  else
    age = 2013 - birthYear
  n = nrow(dataSet)
  dataNA <- (dataSet[,1]+ dataSet[,2])/2
  dataAll <- cbind(dataSet,dataNA)
  #cumulative survival probilities
  if(sex==0)
  data = cumprod(1-dataAll[age:n,3])
  else
    data = cumprod(1-dataAll[age:n,sex])
  
  plotData = c(data, fillZeros(age))
  #create x axis labels
  names(plotData) <- seq(2013,2133)
  barplot(plotData, ylim = c(0, 1), xaxt="n",
          xaxs ="i", yaxs ="i", tcl = -0.2, 
       border = barBorderColor(sex), space = 0.1, width = 1,
       col = colSet(sex)) 
       #col.axis="black" 
  
  title(main = name, col.main = barBorderColor(sex), font.main = 4)
  box(col="dodgerblue")
  lines(plotData, col = lineColSet(sex), lty = "dashed", lwd = 2)   
  
}
plotFamilyMort <- function(dataid) {
  n = nrow(dataid)
 #par(mfrow=c(n,1))
  layout(matrix(1:n, ncol=1))
  names <- dataid$name
  birthYears <- dataid$birthYear
  sexes <- dataid$sex
  for (i in 1:n) {
     par(mar = c(2,2,2,1), mgp = c(1,0.5,0),bg = "gray95")
      barPlotMort(birthYears[i], sexes[i], names[i])
  }
  
  par(new=TRUE,xaxt="s",lab = c(24,5,7))
  axis(1, at=seq(2013,2133,by=10), labels=as.character(seq(2013,2133,by=10)),cex.lab = 0.3)
  axis(2, cex.lab = 0.3)
}



