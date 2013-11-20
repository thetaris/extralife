
require(rCharts)




plotPopulationForecast_rCharts <- function(yearID,nameID,birthID,genderID){

  
  dfDataSet <- data.frame(PopulationForecastDE)
  
  nr = nrow(dfDataSet)
  nc = ncol(dfDataSet)
  
  populationdata <- matrix((nr-1)*(nc-2),nrow=(nr-1),ncol=(nc-2))
  for(i in 1:(nr-1)){
    for(j in 1:(nc-2)){
      populationdata[i,j] <- as.numeric(as.character(gsub(" ","",dfDataSet[i+1,j+2]),
                                           na.rm=TRUE))
    }
  }
  ageGroups <- dfDataSet[1,3:nc]
  ageGroups <- as.vector(t(ageGroups))
  
  genderGroups <- dfDataSet[2:nr,2]
  genderGroups <- as.vector(t(genderGroups))
  
  years <- dfDataSet[2:nr,1]
  years <- as.vector(t(years))
  
  findAgeGroupIndex <- function(age,ageGroups){
    ageGroupSplit<-strsplit(ageGroups," ")
    for(i in 1:length(ageGroups)){
      lower = as.numeric(ageGroupSplit[[i]][1])
      upper = as.numeric(ageGroupSplit[[i]][3])
      if((age>=lower) && (age<upper)){
        return (i)
      }
    }
  }
  
  
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
  namesEducationW = NULL
  dataWorkingW = NULL
  namesWorkingW = NULL
  dataRetiredW = NULL
  namesRetiredW = NULL
  ageGroupEducationW = NULL
  ageGroupWorkingW = NULL
  ageGroupRetiredW = NULL
  
  for (i in seq(1,(nr-1),2)){
    if(yearID==years[i]){ 
      
      dataEducationM <- c(-populationdata[i,1:5], rep(0,(nc-7)))
      namesEducationM<-c(rep("Ausbildung", (nc-2)))
      ageGroupEducationM <- c(ageGroups)
      
      dataWorkingM <- c(rep(0,5),-populationdata[i,6:13], rep(0,(nc-15)))
      namesWorkingM<-c(rep("Arbeit", (nc-2)))
      ageGroupWorkingM <- c(ageGroups)
      
      dataRetiredM <- c(rep(0,13),-populationdata[i,14:(nc-2)])
      namesRetiredM<-c(rep("Rente", (nc-2)))
      ageGroupRetiredM <- c(ageGroups)
      
      dataEducationW <- c(populationdata[i+1,1:5], rep(0,(nc-7)))
      namesEducationW<-c(rep("Ausbildung", (nc-2)))
      ageGroupEducationW <- c(ageGroups)
      
      dataWorkingW <- c(rep(0,5),populationdata[i+1,6:13], rep(0,(nc-15)))
      namesWorkingW<-c(rep("Arbeit", (nc-2)))
      ageGroupWorkingW <- c(ageGroups)
      
      dataRetiredW <- c(rep(0,13),populationdata[i+1,14:(nc-2)])
      namesRetiredW<-c(rep("Rente", (nc-2)))
      ageGroupRetiredW <- c(ageGroups)
      
      dataNameMan= NULL
      namesNameMan = NULL
      ageGroupNameMan = NULL
      dataNameMan0 = NULL  
      
      dataNameWoman= NULL
      namesNameWoman = NULL
      ageGroupNameWoman = NULL
      dataNameWoman0 = NULL  
      for (j in 1:(length(nameID))){ 
        ageID = yearID-birthID[j]
        ageIndex <- findAgeGroupIndex(ageID,ageGroups)
        ageIndex
        if(genderID[j]=="m"){
          namesNameM<-rep(nameID[j],(nc-2))
          ageGroupNameM<-ageGroups
          dataNameM<-c(rep(0,(ageIndex-1)),-populationdata[i,ageIndex], 
                       rep(0,(nc-ageIndex-2)))
          
          #dataNameMan<-c(dataNameM)
          dataNameMan0<-c(rep(0,(nc-2)))
          namesNameMan<-c(namesNameM)
          ageGroupNameMan<-c(ageGroupNameM)
        }
        else{
          if(genderID[j]=="w"){
            namesNameW<-rep(nameID[j],(nc-2))
            ageGroupNameW<-ageGroups
            dataNameW<-c(rep(0,(ageIndex-1)),populationdata[i+1,ageIndex], 
                         rep(0,(nc-ageIndex-2)))
            
            #dataNameWoman<-c(dataNameW)
            dataNameWoman0<-c(rep(0,(nc-2)))
            namesNameWoman<-c(namesNameW)
            ageGroupNameWoman<-c(ageGroupNameW)
          }
        }
      }
      
      for (j in 1:(length(nameID))){ 
        if(genderID[j]=="m"){
          ageID = yearID-birthID[j]
          ageIndex <- findAgeGroupIndex(ageID,ageGroups)
          
          namesNameMan<-c(rep(nameID[j],(nc-2)))
          ageGroupNameMan<-c(ageGroups)
          dataNameMan <- c(rep(0,(ageIndex-1)),-populationdata[i,ageIndex], 
                           rep(0,(nc-ageIndex-2)))
          
          #           dataNameWoman0<-rep(0,(nc-2))
          #           namesNameWoman<-rep(nameID[j],(nc-2))
          #           ageGroupNameWoman<-ageGroups
          
          if(j<length(nameID)){
            for(k in (j+1):length(nameID)){
              if(birthID[j]==birthID[k]){  
                if((ageIndex>=1) && (ageIndex<=5)){
                  if((ageIndex>1) && (ageIndex<5))
                    dataEducationM <-c(-populationdata[i,1:(ageIndex-1)], 0, 
                                       -populationdata[i,(ageIndex+1):5], rep(0,(nc-7)))
                  else{
                    if(ageIndex==1)
                      dataEducationM <-c(0, -populationdata[i,(ageIndex+1):5], rep(0,(nc-7)))
                    else{
                      if(ageIndex==5)
                        dataEducationM <-c(-populationdata[i,1:4],0, rep(0,(nc-7)))
                    }
                  }
                  
                  dataEducationM<-c(dataEducationM,dataNameMan,dataNameWoman0)
                  namesEducationM<-c(namesEducationM,
                                     rep(paste(nameID[j],nameID[k], sep=", "),(nc-2)),
                                     namesNameWoman)
                  ageGroupEducationM<-c(ageGroupEducationM,ageGroupNameMan,ageGroupNameWoman)
                  
                  namesData<-c(namesData,namesEducationM)
                  plotData<-c(plotData,dataEducationM)
                  ageGroup<-c(ageGroup,ageGroupEducationM)
                  j=j+1
                }
                else{
                  if((ageIndex>=6) && (ageIndex<=13)){
                    if((ageIndex>6) && (ageIndex<13))
                      dataWorkingM <- c(rep(0,5),
                                        -populationdata[i,6:(ageIndex-1)], 0,
                                        -populationdata[i,(ageIndex+1):13], 
                                        rep(0,(nc-15)))
                    else{
                      if(ageIndex==6) 
                        dataWorkingM <- c(rep(0,5),
                                          0, -populationdata[i,(ageIndex+1):13], 
                                          rep(0,(nc-15)))
                      else{
                        if(ageIndex==13) 
                          dataWorkingM <- c(rep(0,5),
                                            -populationdata[i,6:(ageIndex-1)], 0,  
                                            rep(0,(nc-15)))
                      }
                    }
                    dataWorkingM<-c(dataWorkingM,dataNameMan,dataNameWoman0)
                    namesWorkingM<-c(namesWorkingM,
                                     rep(paste(nameID[j],nameID[k], sep=", "),(nc-2)),
                                     namesNameWoman)
                    ageGroupWorkingM<-c(ageGroupWorkingM,ageGroupNameMan,ageGroupNameWoman)
                    
                    namesData<-c(namesData,namesWorkingM)
                    plotData<-c(plotData,dataWorkingM)
                    ageGroup<-c(ageGroup,ageGroupWorkingM)
                    j=j+1
                  }
                  else{
                    if((ageIndex>=14) && (ageIndex<=(nc-2))){
                      
                      if((ageIndex>14) && (ageIndex<(nc-2)))
                        dataRetiredM <-c(rep(0,13),
                                         -populationdata[i,14:(ageIndex-1)], 0,
                                         -populationdata[i,(ageIndex+1):(nc-2)])
                      else{
                        if(ageIndex==14)
                          dataRetiredM <-c(rep(0,13),
                                           0, -populationdata[i,(ageIndex+1):(nc-2)])
                        else{
                          if(ageIndex==(nc-2))
                            dataRetiredM <-c(rep(0,13),
                                             -populationdata[i,14:(ageIndex-1)], 0)
                        }
                      }
                      dataRetiredM<-c(dataRetiredM,dataNameMan,dataNameWoman0)
                      namesRetiredM<-c(namesRetiredM,
                                       rep(paste(nameID[j],nameID[k], sep=", "),(nc-2)),
                                       namesNameWoman)
                      ageGroupRetiredM<-c(ageGroupRetiredM,ageGroupNameMan,ageGroupNameWoman)
                      
                      namesData<-c(namesData,namesRetiredM)
                      plotData<-c(plotData,dataRetiredM)
                      ageGroup<-c(ageGroup,ageGroupRetiredM)
                      j=j+1
                    }
                  }
                }
              }
              else{
                if(birthID[j]!=birthID[k]){  
                  if((ageIndex>=1) && (ageIndex<=5)){
                    if((ageIndex>1) && (ageIndex<5))
                      dataEducationM <-c(-populationdata[i,1:(ageIndex-1)], 0, 
                                         -populationdata[i,(ageIndex+1):5], rep(0,(nc-7)))
                    else{
                      if(ageIndex==1)
                        dataEducationM <-c(0, -populationdata[i,(ageIndex+1):5], rep(0,(nc-7)))
                      else{
                        if(ageIndex==5)
                          dataEducationM <-c(-populationdata[i,1:4],0, rep(0,(nc-7)))
                      }
                    }
                    dataEducationM<-c(dataEducationM,dataNameMan,dataNameWoman0)
                    namesEducationM<-c(namesEducationM,namesNameMan,namesNameWoman)
                    ageGroupEducationM<-c(ageGroupEducationM,ageGroupNameMan,ageGroupNameWoman)
                    
                    namesData<-c(namesData,namesEducationM)
                    plotData<-c(plotData,dataEducationM)
                    ageGroup<-c(ageGroup,ageGroupEducationM)
                    
                  }
                  else{
                    if((ageIndex>=6) && (ageIndex<=13)){
                      if((ageIndex>6) && (ageIndex<13))
                        dataWorkingM <- c(rep(0,5),
                                          -populationdata[i,6:(ageIndex-1)], 0,
                                          -populationdata[i,(ageIndex+1):13], 
                                          rep(0,(nc-15)))
                      else{
                        if(ageIndex==6) 
                          dataWorkingM <- c(rep(0,5),
                                            0, -populationdata[i,(ageIndex+1):13], 
                                            rep(0,(nc-15)))
                        else{
                          if(ageIndex==13) 
                            dataWorkingM <- c(rep(0,5),
                                              -populationdata[i,6:(ageIndex-1)], 0,  
                                              rep(0,(nc-15)))
                        }
                      }
                      dataWorkingM<-c(dataWorkingM,dataNameMan,dataNameWoman0)
                      namesWorkingM<-c(namesWorkingM,namesNameMan,namesNameWoman)
                      ageGroupWorkingM<-c(ageGroupWorkingM,ageGroupNameMan,ageGroupNameWoman)
                      
                      namesData<-c(namesData,namesWorkingM)
                      plotData<-c(plotData,dataWorkingM)
                      ageGroup<-c(ageGroup,ageGroupWorkingM)
                      
                    }
                    else{
                      if((ageIndex>=14) && (ageIndex<=(nc-2))){
                        
                        if((ageIndex>14) && (ageIndex<(nc-2)))
                          dataRetiredM <-c(rep(0,13),
                                           -populationdata[i,14:(ageIndex-1)], 0,
                                           -populationdata[i,(ageIndex+1):(nc-2)])
                        else{
                          if(ageIndex==14)
                            dataRetiredM <-c(rep(0,13),
                                             0, -populationdata[i,(ageIndex+1):(nc-2)])
                          else{
                            if(ageIndex==(nc-2))
                              dataRetiredM <-c(rep(0,13),
                                               -populationdata[i,14:(ageIndex-1)], 0)
                          }
                        }
                        dataRetiredM<-c(dataRetiredM,dataNameMan,dataNameWoman0)
                        namesRetiredM<-c(namesRetiredM,namesNameMan,namesNameWoman)
                        ageGroupRetiredM<-c(ageGroupRetiredM,ageGroupNameMan,ageGroupNameWoman)
                        
                        namesData<-c(namesData,namesRetiredM)
                        plotData<-c(plotData,dataRetiredM)
                        ageGroup<-c(ageGroup,ageGroupRetiredM)
                      }
                    }
                  }
                }
              }
            }
          }
          else{ 
            if(j==length(nameID)){
              if(birthID[j-1]!=birthID[j]){
                if((ageIndex>=1) && (ageIndex<=5)){
                  if((ageIndex>1) && (ageIndex<5))
                    dataEducationM <-c(-populationdata[i,1:(ageIndex-1)], 0, 
                                       -populationdata[i,(ageIndex+1):5], rep(0,(nc-7)))
                  else{
                    if(ageIndex==1)
                      dataEducationM <-c(0, -populationdata[i,(ageIndex+1):5], rep(0,(nc-7)))
                    else{
                      if(ageIndex==5)
                        dataEducationM <-c(-populationdata[i,1:4],0, rep(0,(nc-7)))
                    }
                  }
                  
                  dataEducationM<-c(dataEducationM,dataNameMan,dataNameWoman0)
                  namesEducationM<-c(namesEducationM,
                                     namesNameMan,
                                     namesNameWoman)
                  ageGroupEducationM<-c(ageGroupEducationM,ageGroupNameMan,ageGroupNameWoman)
                  
                  namesData<-c(namesData,namesEducationM)
                  plotData<-c(plotData,dataEducationM)
                  ageGroup<-c(ageGroup,ageGroupEducationM)
                  
                }
                else{
                  if((ageIndex>=6) && (ageIndex<=13)){
                    if((ageIndex>6) && (ageIndex<13))
                      dataWorkingM <- c(rep(0,5),
                                        -populationdata[i,6:(ageIndex-1)], 0,
                                        -populationdata[i,(ageIndex+1):13], 
                                        rep(0,(nc-15)))
                    else{
                      if(ageIndex==6) 
                        dataWorkingM <- c(rep(0,5),
                                          0, -populationdata[i,(ageIndex+1):13], 
                                          rep(0,(nc-15)))
                      else{
                        if(ageIndex==13) 
                          dataWorkingM <- c(rep(0,5),
                                            -populationdata[i,6:(ageIndex-1)], 0,  
                                            rep(0,(nc-15)))
                      }
                    }
                    dataWorkingM<-c(dataWorkingM,dataNameMan,dataNameWoman0)
                    namesWorkingM<-c(namesWorkingM,
                                     namesNameMan,
                                     namesNameWoman)
                    ageGroupWorkingM<-c(ageGroupWorkingM,ageGroupNameMan,ageGroupNameWoman)
                    
                    namesData<-c(namesData,namesWorkingM)
                    plotData<-c(plotData,dataWorkingM)
                    ageGroup<-c(ageGroup,ageGroupWorkingM)
                    
                  }
                  else{
                    if((ageIndex>=14) && (ageIndex<=(nc-2))){
                      
                      if((ageIndex>14) && (ageIndex<(nc-2)))
                        dataRetiredM <-c(rep(0,13),
                                         -populationdata[i,14:(ageIndex-1)], 0,
                                         -populationdata[i,(ageIndex+1):(nc-2)])
                      else{
                        if(ageIndex==14)
                          dataRetiredM <-c(rep(0,13),
                                           0, -populationdata[i,(ageIndex+1):(nc-2)])
                        else{
                          if(ageIndex==(nc-2))
                            dataRetiredM <-c(rep(0,13),
                                             -populationdata[i,14:(ageIndex-1)], 0)
                        }
                      }
                      dataRetiredM<-c(dataRetiredM,dataNameMan,dataNameWoman0)
                      namesRetiredM<-c(namesRetiredM,
                                       namesNameMan,
                                       namesNameWoman)
                      ageGroupRetiredM<-c(ageGroupRetiredM,ageGroupNameMan,ageGroupNameWoman)
                      
                      namesData<-c(namesData,namesRetiredM)
                      plotData<-c(plotData,dataRetiredM)
                      ageGroup<-c(ageGroup,ageGroupRetiredM)
                    }
                  }
                }
              }
            }
          }
          
        }
        else{          
          if(genderID[j]=="w"){
            ageID = yearID-birthID[j]
            ageIndex <- findAgeGroupIndex(ageID,ageGroups)
            
            #             namesNameMan<-rep(nameID[j],(nc-2))
            #             ageGroupNameMan<-ageGroups
            #             dataNameMan0 <- rep(0,(nc-2))
            #             
            dataNameWoman<-c(rep(0,(ageIndex-1)),populationdata[i+1,ageIndex], 
                             rep(0,(nc-ageIndex-2)))
            namesNameWoman<-rep(nameID[j],(nc-2))
            ageGroupNameWoman<-ageGroups
            
            if(j<length(nameID)){
              for(k in (j+1):length(nameID)){    
                if(birthID[j]==birthID[k]){  
                  if((ageIndex>=1) && (ageIndex<=5)){
                    if((ageIndex>1) && (ageIndex<5))
                      dataEducationW <-c(populationdata[i+1,1:(ageIndex-1)], 0, 
                                         populationdata[i+1,(ageIndex+1):5], rep(0,(nc-7)))
                    else{
                      if(ageIndex==1)
                        dataEducationW <-c(0, populationdata[i+1,(ageIndex+1):5], rep(0,(nc-7)))
                      else{
                        if(ageIndex==5)
                          dataEducationW <-c(populationdata[i+1,1:4],0, rep(0,(nc-7)))
                      }
                    }
                    
                    dataEducationW<-c(dataEducationW,dataNameWoman,dataNameMan0)
                    namesEducationW<-c(namesEducationW,
                                       rep(paste(nameID[j],nameID[k], sep = " "),(nc-2)),
                                       namesNameMan)
                    ageGroupEducationW<-c(ageGroupEducationW,ageGroupNameWoman,ageGroupNameMan)
                    
                    namesData<-c(namesData,namesEducationW)
                    plotData<-c(plotData,dataEducationW)
                    ageGroup<-c(ageGroup,ageGroupEducationW)
                    j=j+1
                  }
                  else{
                    if((ageIndex>=6) && (ageIndex<=13)){
                      if((ageIndex>6) && (ageIndex<13))
                        dataWorkingW <- c(rep(0,5),
                                          populationdata[i+1,6:(ageIndex-1)], 0,
                                          populationdata[i+1,(ageIndex+1):13], 
                                          rep(0,(nc-15)))
                      else{
                        if(ageIndex==6) 
                          dataWorkingW <- c(rep(0,5),
                                            0, populationdata[i+1,(ageIndex+1):13], 
                                            rep(0,(nc-15)))
                        else{
                          if(ageIndex==13) 
                            dataWorkingW <- c(rep(0,5),
                                              populationdata[i+1,6:(ageIndex-1)], 0,  
                                              rep(0,(nc-15)))
                        }
                      }
                      dataWorkingW<-c(dataWorkingW,dataNameWoman,dataNameMan0)
                      namesWorkingW<-c(namesWorkingW,
                                       rep(paste(nameID[j],nameID[k], sep=" "),(nc-2)),
                                       namesNameMan)
                      ageGroupWorkingW<-c(ageGroupWorkingW,ageGroupNameWoman,ageGroupNameMan)
                      
                      namesData<-c(namesData,namesWorkingW)
                      plotData<-c(plotData,dataWorkingW)
                      ageGroup<-c(ageGroup,ageGroupWorkingW)
                      j=j+1 
                    }
                    else{
                      if((ageIndex>=14) && (ageIndex<=(nc-2))){
                        
                        if((ageIndex>14) && (ageIndex<(nc-2)))
                          dataRetiredW <-c(rep(0,13),
                                           populationdata[i+1,14:(ageIndex-1)], 0,
                                           populationdata[i+1,(ageIndex+1):(nc-2)])
                        else{
                          if(ageIndex==14)
                            dataRetiredW <-c(rep(0,13),
                                             0, populationdata[i+1,(ageIndex+1):(nc-2)])
                          else{
                            if(ageIndex==(nc-2))
                              dataRetiredW <-c(rep(0,13),
                                               populationdata[i+1,14:(ageIndex-1)], 0)
                          }
                        }
                        dataRetiredW<-c(dataRetiredW,dataNameWoman,dataNameMan0)
                        namesRetiredW<-c(namesRetiredW,
                                         rep(paste(nameID[j],nameID[k], sep=" "),(nc-2)),
                                         namesNameMan)
                        ageGroupRetiredW<-c(ageGroupRetiredW,ageGroupNameWoman,ageGroupNameMan)
                        
                        namesData<-c(namesData,namesRetiredW)
                        plotData<-c(plotData,dataRetiredW)
                        ageGroup<-c(ageGroup,ageGroupRetiredW)
                        j=j+1
                      }
                    }
                  }
                }
                else{  
                  if(birthID[j]!=birthID[k]){
                    if((ageIndex>=1) && (ageIndex<=5)){
                      
                      if((ageIndex>1) && (ageIndex<5))
                        dataEducationW <-c(populationdata[i+1,1:(ageIndex-1)], 0, 
                                           populationdata[i+1,(ageIndex+1):5], rep(0,(nc-7)))
                      else{
                        if(ageIndex==1)
                          dataEducationW <-c(0, populationdata[i+1,(ageIndex+1):5], rep(0,(nc-7)))
                        else{
                          if(ageIndex==5)
                            dataEducationW <-c(populationdata[i+1,1:4],0, rep(0,(nc-7)))
                        }
                      }
                      
                      dataEducationW<-c(dataEducationW,dataNameWoman,dataNameMan0)
                      namesEducationW<-c(namesEducationW,namesNameWoman,namesNameMan)
                      ageGroupEducationW<-c(ageGroupEducationW,ageGroupNameWoman,ageGroupNameMan) 
                      
                      namesData<-c(namesData,namesEducationW)
                      plotData<-c(plotData,dataEducationW)
                      ageGroup<-c(ageGroup,ageGroupEducationW)
                      
                    }
                    else{
                      if((ageIndex>=6) && (ageIndex<=13)){
                        
                        if((ageIndex>6) && (ageIndex<13))
                          dataWorkingW <- c(rep(0,5),
                                            populationdata[i+1,6:(ageIndex-1)], 0,
                                            populationdata[i+1,(ageIndex+1):13], 
                                            rep(0,(nc-15)))
                        else{
                          if(ageIndex==6) 
                            dataWorkingW <- c(rep(0,5),
                                              0, populationdata[i+1,(ageIndex+1):13], 
                                              rep(0,(nc-15)))
                          else{
                            if(ageIndex==13) 
                              dataWorkingW <- c(rep(0,5),
                                                populationdata[i+1,6:(ageIndex-1)], 0,  
                                                rep(0,(nc-15)))
                          }
                        }
                        
                        dataWorkingW<-c(dataWorkingW,dataNameWoman,dataNameMan0)
                        namesWorkingW<-c(namesWorkingW,namesNameWoman,namesNameMan)
                        ageGroupWorkingW<-c(ageGroupWorkingW,ageGroupNameWoman,ageGroupNameMan) 
                        
                        namesData<-c(namesData,namesWorkingW)
                        plotData<-c(plotData,dataWorkingW)
                        ageGroup<-c(ageGroup,ageGroupWorkingW)
                        
                      }
                      else{
                        if((ageIndex>=14) && (ageIndex<=(nc-2))){
                          if((ageIndex>14) && (ageIndex<(nc-2)))
                            dataRetiredW <-c(rep(0,13),
                                             populationdata[i+1,14:(ageIndex-1)], 0,
                                             populationdata[i+1,(ageIndex+1):(nc-2)])
                          else{
                            if(ageIndex==14)
                              dataRetiredW <-c(rep(0,13),
                                               0, populationdata[i+1,(ageIndex+1):(nc-2)])
                            else{
                              if(ageIndex==(nc-2))
                                dataRetiredW <-c(rep(0,13),
                                                 populationdata[i+1,14:(ageIndex-1)], 0)
                            }
                          }
                          
                          dataRetiredW<-c(dataRetiredW,dataNameWoman,dataNameMan0)
                          namesRetiredW<-c(namesRetiredW,namesNameWoman,namesNameMan)
                          ageGroupRetiredW<-c(ageGroupRetiredW,ageGroupNameWoman,ageGroupNameMan) 
                          
                          namesData<-c(namesData,namesRetiredW)
                          plotData<-c(plotData,dataRetiredW)
                          ageGroup<-c(ageGroup,ageGroupRetiredW)
                          
                        }
                        
                      }
                    }    
                  }
                }
              }
            }
            else{
              if(j==length(nameID)){
                if(birthID[j-1]!=birthID[j]){
                  if((ageIndex>=1) && (ageIndex<=5)){
                    if((ageIndex>1) && (ageIndex<5))
                      dataEducationW <-c(populationdata[i+1,1:(ageIndex-1)], 0, 
                                         populationdata[i+1,(ageIndex+1):5], rep(0,(nc-7)))
                    else{
                      if(ageIndex==1)
                        dataEducationW <-c(0, populationdata[i+1,(ageIndex+1):5], rep(0,(nc-7)))
                      else{
                        if(ageIndex==5)
                          dataEducationW <-c(populationdata[i+1,1:4],0, rep(0,(nc-7)))
                      }
                    }
                    
                    dataEducationW<-c(dataEducationW,dataNameWoman,dataNameMan0)
                    namesEducationW<-c(namesEducationW,
                                       namesNameWoman,
                                       namesNameMan)
                    ageGroupEducationW<-c(ageGroupEducationW,ageGroupNameWoman,ageGroupNameMan)
                    
                    namesData<-c(namesData,namesEducationW)
                    plotData<-c(plotData,dataEducationW)
                    ageGroup<-c(ageGroup,ageGroupEducationW)
                    
                  }
                  else{
                    if((ageIndex>=6) && (ageIndex<=13)){
                      if((ageIndex>6) && (ageIndex<13))
                        dataWorkingW <- c(rep(0,5),
                                          populationdata[i+1,6:(ageIndex-1)], 0,
                                          populationdata[i+1,(ageIndex+1):13], 
                                          rep(0,(nc-15)))
                      else{
                        if(ageIndex==6) 
                          dataWorkingW <- c(rep(0,5),
                                            0, populationdata[i+1,(ageIndex+1):13], 
                                            rep(0,(nc-15)))
                        else{
                          if(ageIndex==13) 
                            dataWorkingW <- c(rep(0,5),
                                              populationdata[i+1,6:(ageIndex-1)], 0,  
                                              rep(0,(nc-15)))
                        }
                      }
                      dataWorkingW<-c(dataWorkingW,dataNameWoman,dataNameMan0)
                      namesWorkingW<-c(namesWorkingW,
                                       namesNameWoman,
                                       namesNameMan)
                      ageGroupWorkingW<-c(ageGroupWorkingW,ageGroupNameWoman,ageGroupNameMan)
                      
                      namesData<-c(namesData,namesWorkingW)
                      plotData<-c(plotData,dataWorkingW)
                      ageGroup<-c(ageGroup,ageGroupWorkingW)
                      
                    }
                    else{
                      if((ageIndex>=14) && (ageIndex<=(nc-2))){
                        
                        if((ageIndex>14) && (ageIndex<(nc-2)))
                          dataRetiredW <-c(rep(0,13),
                                           populationdata[i+1,14:(ageIndex-1)], 0,
                                           populationdata[i+1,(ageIndex+1):(nc-2)])
                        else{
                          if(ageIndex==14)
                            dataRetiredW <-c(rep(0,13),
                                             0, populationdata[i+1,(ageIndex+1):(nc-2)])
                          else{
                            if(ageIndex==(nc-2))
                              dataRetiredW <-c(rep(0,13),
                                               populationdata[i+1,14:(ageIndex-1)], 0)
                          }
                        }
                        dataRetiredW<-c(dataRetiredW,dataNameWoman,dataNameMan0)
                        namesRetiredW<-c(namesRetiredW,
                                         namesNameWoman,
                                         namesNameMan)
                        ageGroupRetiredW<-c(ageGroupRetiredW,ageGroupNameWoman,ageGroupNameMan)
                        
                        namesData<-c(namesData,namesRetiredW)
                        plotData<-c(plotData,dataRetiredW)
                        ageGroup<-c(ageGroup,ageGroupRetiredW)
                      }
                    }  
                  }
                }
              }
            }
          }
          
          
        }
      }
      
      namesData<-c(namesData,
                   namesEducationM,
                   namesWorkingM,
                   namesRetiredM,
                   namesEducationW,
                   namesWorkingW,
                   namesRetiredW) 
      plotData<-c(plotData,
                  dataEducationM,
                  dataWorkingM,
                  dataRetiredM,
                  dataEducationW,
                  dataWorkingW,
                  dataRetiredW)
      ageGroup<-c(ageGroup,
                  ageGroupEducationM,
                  ageGroupWorkingM,
                  ageGroupRetiredM,
                  ageGroupEducationW,
                  ageGroupWorkingW,
                  ageGroupRetiredW)  
      
    }
  }
  
  rplotData <- data.frame(age=ageGroup,
                          names=namesData,
                          forecast=plotData)
  
  
  rplotData = rplotData[nrow(rplotData):1,]
  rplotData
  
  pf<-nPlot(forecast~age, group="names",
            data = rplotData,
            type = "multiBarHorizontalChart")
  
  pf$chart(tooltipContent = "#! function(key, x, y) { 
           return  '<h3>' + key + '</h3>' +
           '<p>'+'age ' + x + '</p>'+
           '<p>'  + y + 
           ' population'+'</p>'} !#")
  
  
  
  #pf$chart(color= c('#ff7f0e','#1f77b4','#d62728','#bcbd22','#594c26'))
  
  pf$yAxis(showMaxMin=FALSE)
  pf$chart(stacked = 'true')
  
  pf 
  }


yearID = c(2010)
nameID = c("Homer","Medge")
birthID = c(1980,1970)
genderID = c("m","w")


plotPopulationForecast_rCharts(yearID,nameID,birthID,genderID)

#plotPopulationForecast_rCharts(2010,"Homer",1987,"m")

#plotPopulationForecast_rCharts(2025,"Medge",1979,"w")