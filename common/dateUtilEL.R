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
  # Helper functions
    
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