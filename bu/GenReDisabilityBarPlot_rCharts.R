disabilityID <-data.frame(
  age = c(35, 20, 16,50),
  gender = c("m", "f", "m","f"),
  profession_group = c(1,2,3,4)
)
fillZeros <- function(age){
  if (age < 1)
    fillZeros <- c(rep(0, age))
  else
    fillZeros <- c(rep(0, age-1))
}


plotDisability_rCharts <- function(disabilityID){
  
  require(rCharts)
  library(car)
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
  n1=length(dfDataSet1$X)
  n2=length(dfDataSet2$X)
  
  ageDataMale =NULL
  maleGroupData=NULL
  namesMaleGroupData=NULL
  ageDataFemale =NULL
  femaleGroupData=NULL
  namesFemaleGroupData=NULL
  
  ageData=NULL
  GroupData=NULL
  namesGroupData=NULL
  nr =nrow(disabilityID)
  
  for (i in 1:nr){ 
    startIndex=disabilityID$age[i]-15+1
    if(disabilityID$gender[i]=="m"){
      if(disabilityID$profession_group[i]==1){   
        dfDataSet1$Gruppe.1<-as.numeric(as.vector(recode(dfDataSet1$Gruppe.1,"'-'=0.0")))
        maleG1 <- c(cumProb(dfDataSet1$Gruppe.1/1000*1/1.45,
                            disabilityID$age[i])[startIndex:n1]
        )
        names(maleG1)<-c(rep("Male Group 1", length(maleG1)))
        ageMale <-dfDataSet1$X[startIndex:n1]
        
        ageDataMale<-c(ageDataMale,ageMale)
        maleGroupData<-c(maleGroupData,maleG1)
        namesMaleGroupData<-c(namesMaleGroupData,names(maleG1))
        
      }
      else{
        if(disabilityID$profession_group[i]==2){
          
          maleG2 <- c(cumProb(dfDataSet1$Gruppe.2/1000*1/1.45,
                              disabilityID$age[i])[startIndex:n1]
          )
          names(maleG2)<-c(rep("Male Group 2", length(maleG2)))
          ageMale <-dfDataSet1$X[startIndex:n1]
          
          ageDataMale<-c(ageDataMale,ageMale)
          maleGroupData<-c(maleGroupData,maleG2)
          namesMaleGroupData<-c(namesMaleGroupData,names(maleG2))
          
        }
        else{
          if(disabilityID$profession_group[i]==3){
            
            maleG3 <- c(cumProb(dfDataSet1$Gruppe.3/1000*1/1.4,
                                disabilityID$age[i])[startIndex:n1]
            )
            names(maleG3)<-c(rep("Male Group 3", length(maleG3)))
            ageMale <-dfDataSet1$X[startIndex:n1]
            
            ageDataMale<-c(ageDataMale,ageMale)
            maleGroupData<-c(maleGroupData,maleG3)
            namesMaleGroupData<-c(namesMaleGroupData,names(maleG3))
            
          }
          else{
            if(disabilityID$profession_group[i]==4){
              
              maleG4 <- c(cumProb(dfDataSet1$Gruppe.4/1000*1/1.4,
                                  disabilityID$age[i])[startIndex:n1]
              )
              names(maleG4)<-c(rep("Male Group 4", length(maleG4)))
              ageMale <-dfDataSet1$X[startIndex:n1]
              
              ageDataMale<-c(ageDataMale,ageMale)
              maleGroupData<-c(maleGroupData,maleG4)
              namesMaleGroupData<-c(namesMaleGroupData,names(maleG4))
              
            }
          }
        }
      }
    }
    else{
      if(disabilityID$gender[i]=="f"){
        if(disabilityID$profession_group[i]==1){
          
          dfDataSet2$Gruppe.1<-as.numeric(as.vector(recode(dfDataSet2$Gruppe.1,"'-'=0.0")))
          femaleG1 <- c(cumProb(dfDataSet2$Gruppe.1/1000*1/1.45,
                                disabilityID$age[i])[startIndex:n2]
          )
          names(femaleG1)<-c(rep("Female Group 1", length(femaleG1)))
          ageFemale <-dfDataSet2$X[startIndex:n2]
          
          ageDataFemale<-c(ageDataFemale,ageFemale)
          femaleGroupData<-c(femaleGroupData,femaleG1)
          namesFemaleGroupData<-c(namesFemaleGroupData,names(femaleG1))
        }
        else{
          if(disabilityID$profession_group[i]==2){
            
            femaleG2 <- c(cumProb(dfDataSet2$Gruppe.2/1000*1/1.45,
                                  disabilityID$age[i])[startIndex:n2]
            )
            names(femaleG2)<-c(rep("Female Group 2", length(femaleG2)))
            ageFemale <-dfDataSet2$X[startIndex:n2]
            
            ageDataFemale<-c(ageDataFemale,ageFemale)
            femaleGroupData<-c(femaleGroupData,femaleG2)
            namesFemaleGroupData<-c(namesFemaleGroupData,names(femaleG2))
          }
          else{
            if(disabilityID$profession_group[i]==3){
              
              femaleG3 <- c(cumProb(dfDataSet2$Gruppe.3/1000*1/1.42,
                                    disabilityID$age[i])[startIndex:n2]
              )
              names(femaleG3)<-c(rep("Female Group 3", length(femaleG3)))
              ageFemale <-dfDataSet2$X[startIndex:n2]
              
              ageDataFemale<-c(ageDataFemale,ageFemale)
              femaleGroupData<-c(femaleGroupData,femaleG3)
              namesFemaleGroupData<-c(namesFemaleGroupData,names(femaleG3))
            }
            else{
              if(disabilityID$profession_group[i]==4){
                
                femaleG4 <- c(cumProb(dfDataSet2$Gruppe.4/1000*1/1.42,
                                      disabilityID$age[i])[startIndex:n2]
                )
                names(femaleG4)<-c(rep("Female Group 4", length(femaleG4)))
                ageFemale <-dfDataSet2$X[startIndex:n2]
                
                ageDataFemale<-c(ageDataFemale,ageFemale)
                femaleGroupData<-c(femaleGroupData,femaleG4)
                namesFemaleGroupData<-c(namesFemaleGroupData,names(femaleG4))
              }
            }
          }
        }
      }
    }
  }
  
  
  rplotData1<- data.frame(d0=ageDataMale,
                          d1=namesMaleGroupData,
                          d2=maleGroupData)
  rplotData2<- data.frame(d3=ageDataFemale,
                          d4=namesFemaleGroupData,
                          d5=femaleGroupData)
  
  ageData<-c(ageDataMale,ageDataFemale)
  GroupData<-c(maleGroupData,femaleGroupData)
  namesGroupData<-c(namesMaleGroupData,namesFemaleGroupData)
  
  rplotData<-data.frame(ageData,namesGroupData,round(GroupData*100,digits=0))
  
  colnames(rplotData)<-c("age","genderGroup","disabilityProbability")
  
  
  dp<-nPlot(disabilityProbability~age,group="genderGroup",
            data = rplotData, 
            type = "multiBarChart"
  )
  
  dp$chart(tooltipContent = "#! function(key, x, y) { 
           return  '<h3>' + key + '</h3>' +
           '<p>'+'at age ' + x + '</p>'+
           '<p>'  + y + 
           ' disability probability'+'</p>'
           
           
} !#")
  
  
  #dp$chart(color=colorSet)
  
  
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
  
  dp$chart(reduceXTicks = TRUE)
  dp$xAxis(staggerLabels = FALSE)
  dp$yAxis(tickFormat = "#! function(d) {return d +'%'} !#", showMaxMin=FALSE) 
  dp
  
  }

plotDisability_rCharts(disabilityID)
