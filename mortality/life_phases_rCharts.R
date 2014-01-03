lebensphasenChart <- function(data_in){

# Helper functions

years <- function(Date_in){
  as.integer(format(Date_in,format="%Y"))
}

months <- function(Date_in){
  as.integer(format(Date_in,format="%m"))
}

months <- function(Date_in){
  as.integer(format(Date_in,format="%m"))
}

days <- function(Date_in){
  as.integer(format(Date_in,format="%d"))
}

addDate <- function(Date_in, year_add = 0, months_add = 0, days_add = 0){
  years_in = years(Date_in)
  months_in = months(Date_in)
  days_in = days(Date_in)
  
  tmp_months = months_in + months_add
  
  years_in = years_in + year_add + tmp_months %/% 12  
  months_in = tmp_months %% 12
  
  as.Date(ISOdate(years_in , months_in, days_in)) + days_add  
}

rentenEintrittsDatum<-function(Geburtsdatum_in){
  years_in = years(Geburtsdatum_in)
  
  zusatzMonate = min(max(years_in - 1946,0),24) 
  
  addDate(Geburtsdatum_in, year_add = 65, months_add = zusatzMonate)
}


# Data
Names <- as.character(data_in$name)

if (length(Names)<1){
  stop('Keine Personen angelegt oder Daten konnten nicht geladen werden.')
}

DateOfBirth <- as.Date(ISOdate(data_in$birthYear, 1, 1))

Gender <- data_in$sex

WahrscheinlichesSterbedatum = c()
for (i in 1:length(Gender)){

  tmp<-findSurvivalQuantile(DateOfBirth[i], Gender[i], 0.5)  
  if (length( tmp$year)>0){
    WahrscheinlichesSterbedatum[i] = as.numeric(as.Date(ISOdate(tmp$year, 1, 1)))
  }else{
    WahrscheinlichesSterbedatum[i] = 0
  }  
}

# 
# # Chart
Name_frame = c(character())
Phase_frame= c(character())
Dauer_frame= c(numeric())
# 
# 


for(i in 1:length(DateOfBirth)){
  Name_frame[(i-1)*3+1] = Names[i]
  Phase_frame[(i-1)*3+1] = "Ausbildungsphase"
  Dauer_frame[(i-1)*3+1] = max( (as.numeric(DateOfBirth[i] - Sys.Date(), units="days") + 20*365.24 )/365.24, 0)

  Name_frame[(i-1)*3+2] = Names[i]
  Phase_frame[(i-1)*3+2] = "Arbeitsphase"
  Dauer_frame[(i-1)*3+2] = max(as.numeric(rentenEintrittsDatum(DateOfBirth[i]) - Sys.Date(), units="days"),0)/365.25

  Name_frame[(i-1)*3+3] = Names[i]
  Phase_frame[(i-1)*3+3] = "Rentenphase"
  Dauer_frame[(i-1)*3+3] = max(as.numeric(WahrscheinlichesSterbedatum[i] - (as.numeric(Sys.Date(), units="days") + 365.24 *(Dauer_frame[(i-1)*3+1] + Dauer_frame[(i-1)*3+2]) )) /365.24, 0)
  
}



phase = data.frame(Name_frame, Phase_frame, Dauer_frame)
colnames(phase) = c('Name', 'Lebensabschnitt', 'Dauer')

n1 <- nPlot(Dauer ~ Name, data = phase, group = 'Lebensabschnitt', type = 'multiBarHorizontalChart')


n1$addParams(dom = 'life_phases')

n1$chart(stacked = 'true')

n1$chart(tooltip = "#!function(key, x, y, e, graph){ return '<h3>'+ x + '</h3>' +
                '<p>' + key + '</p>' + '<p>' + ((d3.time.format('%Y').parse(y).getTime()-(new Date()).getTime())/1000/60/60/24/365.24).toFixed(0)  
          + ' Jahre</p>' ;}!#")

#n1$chart(width = 700)

n1$yAxis(tickFormat = "#!function(d) {return d3.time.format('%Y')( new Date( (new Date()).getTime() + d * 86400000*365.24 ));}!#")
n1$yAxis(axisLabel = "Zeit in Jahren")

return(n1)
}