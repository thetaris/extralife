familyID <-data.frame(
  name = c("Smino","Katie","EmptyID"),
  birthYear = c(1960,2010,0),
  # 1 for male, 2 for female, 0 for na
  sex = c(1,2,0)
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
  n = nrow(dataSet)
  
  
  #if sex=0, use dataNA
  dataNA <- (dataSet[,1]+ dataSet[,2])/2
  
  dataAll <- cbind(dataSet,dataNA)
  
  nr = nrow(familyID)
  plotData = NULL
  plotDataNames = NULL
  ageData = NULL
  colorSet = NULL
  ages = NULL
  for (i in 1:nr){
    if(familyID$birthYear[i]==0){

      ageDataEmpty <-c(seq(2013,n-1+2013))
      
      probEmptyStudyPhase <- rep(0,n)
      namesProbEmptyStudyPhase <- paste(rep(familyID$name[i],n),
                                        "Ausbildungsphase", sep = ":")
      probEmptyWorkPhase <- rep(0,n)
      namesProbEmptyWorkPhase <- paste(rep(familyID$name[i],n),
                                       "Arbeitsphase", sep = ":")
      probEmptyRetirePhase <- rep(0,n)
      namesProbEmptyRetirePhase <- paste(rep(familyID$name[i],n),
                                         "Rentenphase", sep = ":")
      
      plotData<-c(plotData,
                  probEmptyStudyPhase,
                  probEmptyWorkPhase,
                  probEmptyRetirePhase) 
      plotDataNames<-c(plotDataNames,
                       namesProbEmptyStudyPhase,
                       namesProbEmptyWorkPhase,
                       namesProbEmptyRetirePhase)
      ageData<-c(ageData,ageDataEmpty)
      colorSet<-c(colorSet, 'yellow')
    }
    else{
      if(familyID$birthYear[i] == 2013)
        ages[i] = 2013 - familyID$birthYear[i] +1
      else
        ages[i] = 2013 - familyID$birthYear[i] 
      
      #male mortality probs 
      if(familyID$sex[i]==1){ 
        probMale <- c(cumprod(1-dataAll[ages[i]:n,1]),fillZeros(ages[i]))
        namesProbMale <- (rep(familyID$name[i],length(probMale)))
        
        ageDataMale <- c(seq(2013,n+2013-1))
        #if ages is within work phase        
        if((ages[i]>23) & (ages[i]<=67)){
          
          probMaleWorkPhase <- c(probMale[1:(68-ages[i])],
                                 rep(0,(ages[i]-1)),
                                 rep(0,(n-67)))
          namesProbMaleWorkPhase <- paste(rep(familyID$name[i],n),
                                          "Arbeitsphase", sep = ":")
          
          probMaleRetirePhase <- c(rep(0,(68-ages[i])),
                                   probMale[(68-ages[i]):(n-ages[i])],
                                   rep(0,ages[i]-1))
          namesProbMaleRetirePhase <- paste(rep(familyID$name[i],n),
                                            "Rentenphase", sep = ":")
          
          probMaleStudyPhase <- rep(0,n)
          namesProbMaleStudyPhase <- paste(rep(familyID$name[i],n),
                                           "Ausbildungsphase", sep = ":")
          
          
          plotData<-c(plotData,
                      probMaleStudyPhase,
                      probMaleWorkPhase,
                      probMaleRetirePhase)
          plotDataNames<-c(plotDataNames,
                           namesProbMaleStudyPhase,
                           namesProbMaleWorkPhase,
                           namesProbMaleRetirePhase)
          ageData<-c(ageData,ageDataMale)

          colorSetMale<-c('#17becf','#1f77b4')
          colorSet<-c(colorSet,colorSetMale)
        }
        else{#if age is within the education phase
          if(ages[i]<=23){
            
            probMaleStudyPhase <- c(probMale[1:(24-ages[i])],
                                  rep(0,ages[i]-1), 
                                  rep(0,n-23))
            namesProbMaleStudyPhase <- paste(rep(familyID$name[i],n),
                                             "Ausbildungsphase", sep = ":")
            
            probMaleWorkPhase <- c(rep(0,(24-ages[i])),
                                   probMale[(24-ages[i]):(67-ages[i])],
                                   rep(0,ages[i]-1), 
                                   rep(0,(n-67)))
            namesProbMaleWorkPhase <- paste(rep(familyID$name[i],n),
                                            "Arbeitsphase", sep = ":")
            
            probMaleRetirePhase <- c(rep(0,(68-ages[i])),
                                     probMale[(68-ages[i]):(n-ages[i])],
                                     rep(0,ages[i]-1))
            namesProbMaleRetirePhase <- paste(rep(familyID$name[i],n),
                                              "Rentenphase", sep = ":")
            
            plotData<-c(plotData,
                        probMaleStudyPhase,
                        probMaleWorkPhase,
                        probMaleRetirePhase)
            
            plotDataNames<-c(plotDataNames,
                             namesProbMaleStudyPhase,
                             namesProbMaleWorkPhase,
                             namesProbMaleRetirePhase)
            ageData<-c(ageData,ageDataMale)
            
            colorSetMale<-c('#ff7f0e','#17becf','#1f77b4')
            colorSet<-c(colorSet,colorSetMale)
          }
          else{
            if(ages[i]>67){#if age is within the retirement phase
              
              probMaleRetirePhase <- c(probMale[1:(n+1-ages[i])],
                                     rep(0,(ages[i]-1)))
              namesProbMaleRetirePhase <- paste(rep(familyID$name[i],n),
                                                "Rentenphase", sep = ":")
              
              probMaleStudyPhase <- rep(0,n)
              namesProbMaleStudyPhase <- paste(rep(familyID$name[i],n),
                                               "Ausbildungsphase", sep = ":")
              
              probMaleWorkPhase <- rep(0,n)
              namesProbMaleWorkPhase <- paste(rep(familyID$name[i],n),
                                              "Arbeitsphase",sep = ":")
              
              plotData<-c(plotData,
                          probMaleStudyPhase,
                          probMaleWorkPhase,
                          probMaleRetirePhase)
              
              plotDataNames<-c(plotDataNames,
                               namesProbMaleStudyPhase,
                               namesProbMaleWorkPhase,
                               namesProbMaleRetirePhase)
              ageData<-c(ageData,ageDataMale)
              
              colorSetMale<-c('#1f77b4')
              colorSet<-c(colorSet,colorSetMale)
              
            }
          }
          
        }

      }
      else{#female mortality probs
        if(familyID$sex[i]==2){
          probFemale<- c(cumprod(1-dataAll[ages[i]:n,2]),fillZeros(ages[i]))
          namesProbFemale<-(rep(familyID$name[i],length(probFemale)))
          
          ageDataFemale<-c(seq(2013,n+2013-1))
          #if age is within the work phase
          if((ages[i]>23)&(ages[i]<=67)){
            
            probFemaleWorkPhase <- c(probFemale[1:(68-ages[i])],
                                     rep(0,(ages[i]-1)),
                                     rep(0,(n-67)))
            namesProbFemaleWorkPhase <- paste(rep(familyID$name[i],n),
                                              "Arbeitsphase", sep = ":")
            
            probFemaleRetirePhase <- c(rep(0,(68-ages[i])),
                                       probFemale[(68-ages[i]):(n-ages[i])],
                                       rep(0,ages[i]-1))
                                     
            namesProbFemaleRetirePhase <- paste(rep(familyID$name[i],n),
                                                "Rentenphase", sep = ":")
            
            probFemaleStudyPhase <- rep(0,n)
            namesProbFemaleStudyPhase <- paste(rep(familyID$name[i],n),
                                               "Ausbildungsphase", sep = ":")
            
            plotData<-c(plotData,
                        probFemaleStudyPhase,
                        probFemaleWorkPhase,
                        probFemaleRetirePhase)           
            plotDataNames<-c(plotDataNames,
                             namesProbFemaleStudyPhase,
                             namesProbFemaleWorkPhase,
                             namesProbFemaleRetirePhase)                        
            ageData<-c(ageData,ageDataFemale)
            
            colorSetFemale<-c('#ff2385', '#d62728')
            colorSet<-c(colorSet,colorSetFemale)
            
          }
          else{
            if(ages[i]<=23){#if age is within the education phase
              
              probFemaleStudyPhase <- c(probFemale[1:(24-ages[i])],
                                        rep(0,ages[i]-1), 
                                        rep(0,n-23))
              namesProbFemaleStudyPhase<-paste(rep(familyID$name[i],n),
                                               "Ausbildungsphase", sep = ":")
              
              probFemaleWorkPhase <- c(rep(0,(24-ages[i])),
                                      probFemale[(24-ages[i]):(67-ages[i])],
                                      rep(0,ages[i]-1), 
                                      rep(0,(n-67)))
              namesProbFemaleWorkPhase <- paste(rep(familyID$name[i],n),
                                                "Arbeitsphase", sep = ":")
              
              probFemaleRetirePhase <- c(rep(0,(68-ages[i])),
                                         probFemale[(68-ages[i]):(n-ages[i])],
                                         rep(0,ages[i]-1))
                                      
              namesProbFemaleRetirePhase <- paste(rep(familyID$name[i],n),
                                                  "Rentenphase", sep = ":")
              
              plotData<-c(plotData,
                          probFemaleStudyPhase,
                          probFemaleWorkPhase,
                          probFemaleRetirePhase)
              plotDataNames<-c(plotDataNames,
                               namesProbFemaleStudyPhase,
                               namesProbFemaleWorkPhase,
                               namesProbFemaleRetirePhase)
              ageData<-c(ageData,ageDataFemale)
              
              colorSetFemale<-c('#e377c2','#ff2385', '#d62728')
              colorSet<-c(colorSet,colorSetFemale)
            }
            else{
              if(ages[i]>67){#if age is within the retirement phase
                
                probFemaleRetirePhase <- c(probFemale[1:(n+1-ages[i])],
                                           rep(0,(ages[i]-1)))
                namesProbFemaleRetirePhase <- paste(rep(familyID$name[i],n),
                                                    "Rentenphase", sep = ":")
                
                probFemaleStudyPhase <- rep(0,n)
                namesProbFemaleStudyPhase <- paste(rep(familyID$name[i],n),
                                                   "Ausbildungsphase",sep = ":")
                
                probFemaleWorkPhase <- rep(0,n)
                namesProbFemaleWorkPhase <- paste(rep(familyID$name[i],n),
                                                  "Arbeitsphase",sep = ":")
                
                plotData<-c(plotData,
                            probFemaleStudyPhase,
                            probFemaleWorkPhase,
                            probFemaleRetirePhase)
                plotDataNames<-c(plotDataNames,
                                 namesProbFemaleStudyPhase,
                                 namesProbFemaleWorkPhase,
                                 namesProbFemaleRetirePhase)
                ageData<-c(ageData,ageDataFemale)
                
                colorSetFemale<-c('#d62728')
                colorSet<-c(colorSet,colorSetFemale)
              }
            }
            
          }

        }
        else{#when sex == na
          if(familyID$sex[i]==0){
            probNeutral<- c(cumprod(1-dataAll[ages[i]:n,3]),fillZeros(ages[i]))
            namesProbNeutral<-(rep(familyID$name[i],length(probNeutral)))
            
            ageDataNeutral<-c(seq(2013,n+2013-1))
            
            if((ages[i]>23)&(ages[i]<=67)){
              
              probNeutralWorkPhase <- c(probNeutral[1:(68-ages[i])],
                                      rep(0,(ages[i]-1)),
                                      rep(0,(n-67)))
              namesProbNeutralWorkPhase<-paste(rep(familyID$name[i],n),
                                                 "Arbeitsphase", sep = ":")
              
              probNeutralRetirePhase<-c(rep(0,(68-ages[i])),
                                        probFemale[(68-ages[i]):(n-ages[i])],
                                        rep(0,ages[i]-1))
                                       
              namesProbNeutralRetirePhase<-paste(rep(familyID$name[i],n),
                                                 "Rentenphase", sep = ":")
              
              probNeutralStudyPhase<-rep(0,n)
              namesProbNeutralStudyPhase<-paste(rep(familyID$name[i],n),
                                                "Ausbildungsphase", sep = ":")
              
              plotData<-c(plotData,
                          probNeutralStudyPhase,
                          probNeutralWorkPhase,
                          probNeutralRetirePhase)
              
              plotDataNames<-c(plotDataNames,
                               namesProbNeutralStudyPhase,
                               namesProbNeutralWorkPhase,
                               namesProbNeutralRetirePhase)
              
              ageData<-c(ageData,ageDataNeutral)
              
              colorSetNeutral<-c('#229922','#2ca02c') 
              colorSet<-c(colorSet,colorSetNeutral)
            }
            else{
              if(ages[i]<=23){
                
                probNeutralStudyPhase <- c(probNeutra[1:(24-ages[i])],
                                           rep(0,ages[i]-1), 
                                           rep(0,n-23))
                namesProbNeutralStudyPhase <- paste(rep(familyID$name[i],n),
                                                    "Ausbildungsphase", sep = ":")
                
                probNeutralWorkPhase <- c(rep(0,(24-ages[i])),
                                          probNeutral[(24-ages[i]):(67-ages[i])],
                                          rep(0,ages[i]-1), 
                                          rep(0,(n-67)))
                namesProbNeutralWorkPhase <- paste(rep(familyID$name[i],n),
                                                   "Arbeitsphase", sep = ":")
                
                probNeutralRetirePhase <- c(rep(0,(68-ages[i])),
                                            probNeutral[(68-ages[i]):(n-ages[i])],
                                            rep(0,ages[i]-1))
                namesProbNeutralRetirePhase<-paste(rep(familyID$name[i],n),
                                                   "Rentenphase", sep = ":")
                
                plotData<-c(plotData,
                            probNeutralStudyPhase,
                            probNeutralWorkPhase,
                            probNeutralRetirePhase)
                
                plotDataNames<-c(plotDataNames,
                                 namesProbNeutralStudyPhase,
                                 namesProbNeutralWorkPhase,
                                 namesProbNeutralRetirePhase)
                
                ageData<-c(ageData,ageDataNeutral)
                
                colorSetNeutral<-c('#bcbd22','#229922','#2ca02c') 
                colorSet<-c(colorSet,colorSetNeutral)
              }
              else{
                if(ages[i]>67){
                  
                  probNeutralRetirePhase <- c(probNeutral[1:(n+1-ages[i])],
                                              rep(0,(ages[i]-1)))
                  namesProbNeutralRetirePhase<-paste(rep(familyID$name[i],n),
                                                     "Rentenphase", sep = ":")
                  probNeutralStudyPhase<-rep(0,n)
                  namesProbNeutralStudyPhase<-paste(rep(familyID$name[i],n),
                                                   "Ausbildungsphase", sep = ":")
                  
                  probNeutralWorkPhase<-rep(0,n)
                  namesProbNeutralWorkPhase<-paste(rep(familyID$name[i],n),
                                                  "Arbeitsphase", sep = ":")
                  
                  plotData<-c(plotData,
                              probNeutralStudyPhase,
                              probNeutralWorkPhase,
                              probNeutralRetirePhase)
                  
                  plotDataNames<-c(plotDataNames,
                                   namesProbNeutralStudyPhase,
                                   namesProbNeutralWorkPhase,
                                   namesProbNeutralRetirePhase)
                  
                  ageData<-c(ageData,ageDataNeutral)
                  
                  colorSetNeutral<-c('#2ca02c') 
                  colorSet<-c(colorSet,colorSetNeutral)
                }
              }
              
            }
            
          
          }
        }
      }
      
    } 
    
  } 
  
  
  rplotData0<- data.frame(d0=ageData)
  rplotData1 <- data.frame(d1=plotDataNames,d2=plotData)
  
  dfplotdata <- data.frame(rplotData0,rplotData1)
  dfplotdata
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
  
  
  np
  
  }


plotMortality_rCharts(familyID)