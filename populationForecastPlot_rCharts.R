
require(rCharts)

dfDataSet <- data.frame(PopulationForecastDE)

nr = nrow(dfDataSet)
nc = ncol(dfDataSet)

data <- matrix((nr-1)*(nc-2),nrow=(nr-1),ncol=(nc-2))
for(i in 1:(nr-1)){
  for(j in 1:(nc-2)){
    data[i,j] <- as.numeric(as.character(gsub(" ","",dfDataSet[i+1,j+2]),
                                         na.rm=TRUE))
  }
}
ageGroups <- dfDataSet[1,3:nc]
ageGroups <- as.vector(t(ageGroups))


plotPopulationForecast_rCharts <- function(year){
  
  namesData = NULL
  plotData = NULL
  ageGroup = NULL
  
  dataEducationM = NULL
  namesEducationM= NULL
  dataWorkingM = NULL
  namesWorkingM= NULL
  dataRetiredM = NULL
  namesRetiredM= NULL
  ageGroupEducationM  =NULL
  ageGroupWorkingM=NULL
  ageGroupRetiredM=NULL
  
  dataEducationW = NULL
  namesEducationW= NULL
  dataWorkingW = NULL
  namesWorkingW= NULL
  dataRetiredW = NULL
  namesRetiredW= NULL
  ageGroupEducationW=NULL
  ageGroupWorkingW=NULL
  ageGroupRetiredW=NULL
  for (i in 2:nr){
    if(year==dfDataSet[i,1]){  
      if(dfDataSet[i,2]=="m"){ 
        dataEducationM <- c(-data[i-1,1:5])
        namesEducationM<-rep("Education", 5)
        ageGroupEducationM <- c(ageGroups[1:5])
        
        dataWorkingM <- c(-data[i-1,6:13])
        namesWorkingM<-rep("Working", 8)
        ageGroupWorkingM <- c(ageGroups[6:13])
        
        dataRetiredM <- c(-data[i-1,14:(nc-2)])
        namesRetiredM<-rep("Retired", (nc-15))
        ageGroupRetiredM <- c(ageGroups[14:(nc-2)])
        
        namesData<-c(namesData,
                     namesEducationM,
                     namesWorkingM,
                     namesRetiredM)                
        
        plotData<-c(plotData,
                    dataEducationM,
                    dataWorkingM,
                    dataRetiredM)
        ageGroup<-c(ageGroup,
                    ageGroupEducationM,
                    ageGroupWorkingM,
                    ageGroupRetiredM
        )
        
      }
      else{
        dataEducationW <- c(dataEducationW,data[i-1,1:5])
        namesEducationW<-rep("Education", 5)
        ageGroupEducationW <- c(ageGroups[1:5])
        
        dataWorkingW <- c(dataWorkingW,data[i-1,6:13])
        namesWorkingW<-rep("Working", 8)
        ageGroupWorkingW <- c(ageGroups[6:13])
        
        dataRetiredW <- c(dataRetiredW,data[i-1,14:(nc-2)])
        namesRetiredW<-rep("Retired", (nc-15))
        ageGroupRetiredW <- c(ageGroups[14:(nc-2)])
        
        namesData<-c(namesData,
                     namesEducationW,
                     namesWorkingW,
                     namesRetiredW)                
        
        plotData<-c(plotData,
                    dataEducationW,
                    dataWorkingW,
                    dataRetiredW)
        ageGroup<-c(ageGroup,
                    ageGroupEducationW,
                    ageGroupWorkingW,
                    ageGroupRetiredW)
      }        
      
    }
    
  }
  rplotData <- data.frame(age=c(ageGroup),
                          names=c(namesData),
                          forecast=(plotData))
  
  
  rplotData = rplotData[nrow(rplotData):1,]
  rplotData
  
  pf<-nPlot(forecast~age, group="names",
            data = rplotData, 
            type = "multiBarHorizontalChart"
            
  )
  
  pf$chart(tooltipContent = "#! function(key, x, y) { 
           return  '<h3>' + key + '</h3>' +
           '<p>'+'age ' + x + '</p>'+
           '<p>'  + y + 
           ' population'+'</p>'
           
           
} !#")
  
  
  
  pf$chart(color= c('#ff7f0e','#1f77b4','#d62728'))
  
  pf$yAxis(showMaxMin=FALSE)
  
  pf 
  
  }


plotPopulationForecast_rCharts(2010)


