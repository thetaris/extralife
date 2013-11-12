familyID <-data.frame(
  name = c("Smino","Katie","Anette","David","EmptyID"),
  birthYear = c(1920,1976,1984,2012,0),
  # 1 for male, 2 for female, 0 for na
  sex = c(1,2,2,1,0)
)
fillZeros <- function(age){
  if (age < 1)
    fillZeros <- c(rep(0, age))
  else
    fillZeros <- c(rep(0, age-1))
}

plotMortality_rCharts <- function(familyID){
  
  require(rCharts)
  #require(reshape2)
  #get current working directory
  wd <- getwd()
  #file name
  fileName <- "data/DEU_1D_InsMort_prob_ERGO_Zielbild_Leben.dat"
  readName <- paste(wd, fileName, sep = "/")
  #read data file
  dataSet <- read.csv(readName, sep = "\t")
  
  
  #if sex=0, use dataNA
  dataNA <- (dataSet[,1]+ dataSet[,2])/2
  
  dataAll <- cbind(dataSet,dataNA)
  
  nr =nrow(familyID)
  plotData=NULL
  plotDataNames=NULL
  ageData=NULL
  colorSet=NULL
  ages=NULL
  for (i in 1:nr){
    if(familyID$birthYear[i]==0){
      probEmpty<- rep(0,n)
      names(probEmpty)<-rep("emptyData",length(probEmpty))
      ageDataEmpty <-c(seq(2013,n-1+2013))
      
      
      plotData<-c(plotData,probEmpty) 
      plotDataNames<-c(plotDataNames,names(probEmpty))
      ageData<-c(ageData,ageDataEmpty)
      colorSet<-c(colorSet, 'yellow')
    }
    else{
      if(familyID$birthYear[i] == 2013)
        ages[i] = 2013 - familyID$birthYear[i] +1
      else
        ages[i] = 2013 - familyID$birthYear[i] 
      
      
      if(familyID$sex[i]==1){  
        probMale<- c(cumprod(1-dataAll[ages[i]:n,1]),fillZeros(ages[i]))
        names(probMale)<-(rep(familyID$name[i],length(probMale)))
        
        
        if((ages[i]>23)&(ages[i]<=67)){
          
          probMaleWorkPhase<- probMale[1:(67-ages[i])]
          names(probMaleWorkPhase)<-paste(names(probMale)[1:(67-ages[i])],
                                          "Arbeitsphase",sep = ":")
          
          probMaleRetirePhase<-probMale[(68-ages[i]):n]
          names(probMaleRetirePhase)<-paste(names(probMale)[(68-ages[i]):n],
                                            "Rentenphase",sep = ":")
          
          plotData<-c(plotData,
                      probMaleWorkPhase,
                      probMaleRetirePhase
          )
          plotDataNames<-c(plotDataNames,
                           names(probMaleWorkPhase),
                           names(probMaleRetirePhase)
          )
          colorSetMale<-c('#17becf','#1f77b4')
          colorSet<-c(colorSet,colorSetMale)
        }
        else{
          if(ages[i]<=23){
            
            probMaleStudyPhase<-probMale[1:(23-ages[i])]
            names(probMaleStudyPhase)<-paste(names(probMale)[1:(23-ages[i])],
                                             "Ausbildungsphase",sep = ":")
            
            probMaleWorkPhase<- probMale[(24-ages[i]):67]
            names(probMaleWorkPhase)<-paste(names(probMale)[(24-ages[i]):67],
                                            "Arbeitsphase",sep = ":")
            
            probMaleRetirePhase<-probMale[68:n]
            names(probMaleRetirePhase)<-paste(names(probMale)[68:n],
                                              "Rentenphase",sep = ":")
            
            plotData<-c(plotData,
                        probMaleStudyPhase,
                        probMaleWorkPhase,
                        probMaleRetirePhase
            )
            plotDataNames<-c(plotDataNames,
                             names(probMaleStudyPhase),
                             names(probMaleWorkPhase),
                             names(probMaleRetirePhase)
            )
            colorSetMale<-c('#ff7f0e','#17becf','#1f77b4')
            colorSet<-c(colorSet,colorSetMale)
          }
          else{
            if(ages[i]>67){
              
              probMaleRetirePhase<-probMale[1:n]
              names(probMaleRetirePhase)<-paste(names(probMale)[1:n],
                                                "Rentenphase")
              
              plotData<-c(plotData,
                          probMaleRetirePhase
              )
              plotDataNames<-c(plotDataNames,
                               names(probMaleRetirePhase)
              )
              colorSetMale<-c('#1f77b4')
              colorSet<-c(colorSet,colorSetMale)
              
            }
          }
          
        }
        ageDataMale<-c(seq(2013,n+2013-1))
        ageData<-c(ageData,ageDataMale)
      }
      else{
        if(familyID$sex[i]==2){
          probFemale<- c(cumprod(1-dataAll[ages[i]:n,2]),fillZeros(ages[i]))
          names(probFemale)<-(rep(familyID$name[i],length(probFemale)))
          
          if((ages[i]>23)&(ages[i]<=67)){
            
            probFemaleWorkPhase<- probFemale[1:(67-ages[i])]
            names(probFemaleWorkPhase)<-paste(names(probFemale)[1:(67-ages[i])],
                                              "Arbeitsphase")
            
            probFemaleRetirePhase<-probFemale[(68-ages[i]):n]
            names(probFemaleRetirePhase)<-paste(names(probFemale)[(68-ages[i]):n],
                                                "Rentenphase")
            
            
            plotData<-c(plotData,
                        probFemaleWorkPhase,
                        probFemaleRetirePhase
            )
            plotDataNames<-c(plotDataNames,
                             names(probFemaleWorkPhase),
                             names(probFemaleRetirePhase)
            )
            colorSetFemale<-c('#ff2385', '#d62728')
            colorSet<-c(colorSet,colorSetFemale)
            
          }
          else{
            if(ages[i]<=23){
              
              probFemaleStudyPhase<-probFemale[1:(23-ages[i])]
              names(probFemaleStudyPhase)<-paste(names(probFemale)[1:(23-ages[i])],
                                                 "Ausbildungsphase")
              
              probFemaleWorkPhase<- probFemale[(24-ages[i]):67]
              names(probFemaleWorkPhase)<-paste(names(probFemale)[(24-ages[i]):67],
                                                "Arbeitsphase")
              
              probFemaleRetirePhase<-probFemale[68:n]
              names(probFemaleRetirePhase)<-paste(names(probFemale)[68:n],
                                                  "Rentenphase")
              
              plotData<-c(plotData,
                          probFemaleStudyPhase,
                          probFemaleWorkPhase,
                          probFemaleRetirePhase
              )
              plotDataNames<-c(plotDataNames,
                               names(probFemaleStudyPhase),
                               names(probFemaleWorkPhase),
                               names(probFemaleRetirePhase)
              )
              colorSetFemale<-c('#e377c2','#ff2385', '#d62728')
              colorSet<-c(colorSet,colorSetFemale)
            }
            else{
              if(ages[i]>67){
                
                probFemaleRetirePhase<-probFemale[1:n]
                names(probFemaleRetirePhase)<-paste(names(probFemale)[1:n],
                                                    "Rentenphase")
                
                plotData<-c(plotData,
                            probFemaleRetirePhase
                )
                plotDataNames<-c(plotDataNames,
                                 names(probFemaleRetirePhase)
                )
                colorSetFemale<-c('#d62728')
                colorSet<-c(colorSet,colorSetFemale)
              }
            }
            
          }
          ageDataFemale<-c(seq(2013,n+2013-1))
          ageData<-c(ageData,ageDataFemale)
        }
        else{
          if(familyID$sex[i]==0){
            probNeutral<- c(cumprod(1-dataAll[ages[i]:n,3]),fillZeros(ages[i]))
            names(probNeutral)<-(rep(familyID$name[i],length(probNeutral)))
            
            
            if((ages[i]>23)&(ages[i]<=67)){
              
              probNeutralWorkPhase<- probNeutral[1:(67-ages[i])]
              names(probNeutralWorkPhase)<-paste(names(probNeutral)[1:(67-ages[i])],
                                                 "Arbeitsphase")
              
              probNeutralRetirePhase<-probNeutral[(68-ages[i]):n]
              names(probNeutralRetirePhase)<-paste(names(probNeutral)[(68-ages[i]):n],
                                                   "Rentenphase")
              
              
              plotData<-c(plotData,
                          probNeutralWorkPhase,
                          probNeutralRetirePhase
              )
              plotDataNames<-c(plotDataNames,
                               names(probNeutralWorkPhase),
                               names(probNeutralRetirePhase)
              )
              colorSetNeutral<-c('#229922','#2ca02c') 
              colorSet<-c(colorSet,colorSetNeutral)
            }
            else{
              if(ages[i]<=23){
                
                probNeutralStudyPhase<-probNeutral[1:(23-ages[i])]
                names(probNeutralStudyPhase)<-paste(names(probNeutral)[1:(23-ages[i])],
                                                    "Ausbildungsphase")
                
                probNeutralWorkPhase<- probNeutral[(24-ages[i]):67]
                names(probNeutralWorkPhase)<-paste(names(probNeutral)[(24-ages[i]):67],
                                                   "Arbeitsphase")
                
                probNeutralRetirePhase<-probNeutral[68:n]
                names(probNeutralRetirePhase)<-paste(names(probNeutral)[68:n],
                                                     "Rentenphase")
                
                plotData<-c(plotData,
                            probNeutralStudyPhase,
                            probNeutralWorkPhase,
                            probNeutralRetirePhase
                )
                plotDataNames<-c(plotDataNames,
                                 names(probNeutralStudyPhase),
                                 names(probNeutralWorkPhase),
                                 names(probNeutralRetirePhase)
                )
                colorSetNeutral<-c('#bcbd22','#229922','#2ca02c') 
                colorSet<-c(colorSet,colorSetNeutral)
              }
              else{
                if(ages[i]>67){
                  
                  probNeutralRetirePhase<-probNeutral[1:n]
                  names(probNeutralRetirePhase)<-paste(names(probNeutral)[1:n],
                                                       "Rentenphase")
                  
                  plotData<-c(plotData,
                              probNeutralRetirePhase
                  )
                  plotDataNames<-c(plotDataNames,
                                   names(probNeutralRetirePhase)
                  )
                  colorSetNeutral<-c('#2ca02c') 
                  colorSet<-c(colorSet,colorSetNeutral)
                }
              }
              
            }
            
            
            ageDataNeutral<-c(seq(2013,n+2013-1))
            ageData<-c(ageData,ageDataNeutral)
          }
        }
      }
      
    } 
    
  } 
  
  
  rplotData0<- data.frame(d0=ageData)
  rplotData1 <- data.frame(d1=plotDataNames,d2=plotData)
  
  dfplotdata <- data.frame(rplotData0,rplotData1)
  
  colnames(dfplotdata)<-c("age","gender","cumulativeProbability")
  
  np<-nPlot(cumulativeProbability~age,group="gender",data = dfplotdata, 
            type = "multiBarChart"
  )
  
  np$chart(tooltipContent = "#! function(key, x, y) { 
           return  '<h3>' + key + '</h3>' +
           '<p>'  + y*100 + '%' +
           ' survival probability'+'</p>'+
           '<p>'+' in year ' + x + '</p>'
           
} !#")
  
  np$chart(color=colorSet)
  
  
  #np$chart(color= c(rep('#ff7f0e',9))) #orange
  #np$chart(color= c(rep('#2ca02c'',9))) #green
  #np$chart(color= c(rep('#1f77b4',9))) #blue
  #np$chart(color= c(rep('#d62728',9))) #red
  #np$chart(color= c(rep('#8c564b',9))) #red2
  #np$chart(color= c(rep('#e377c2',9))) #violet
  #np$chart(color= c(rep('#bcbd22',9))) #grass green
  #np$chart(color= c(rep('#17becf',9)))  #greenish blue
  #np$chart(color= c(rep('#ff2385',9))) #deep violet
  #np$chart(color= c(rep('#229922',9))) #deep green
  
  #color= c('#ff7f0e','#17becf','#1f77b4') #male colors
  #color= c('#e377c2','#ff2385', '#d62728') #female colors
  #color=c('#bcbd22','#229922','#2ca02c') #neutral colors
  
  
  np$chart(reduceXTicks = TRUE)
  np$xAxis(staggerLabels = FALSE, showMaxMin=TRUE)
  
  
  return(np)
  
  }


plotMortality_rCharts(familyID)