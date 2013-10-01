Name_frame = c(character())
Einkommen_frame= c(character())
Wert_frame= c(numeric())

Names = c(character())
Names[1] = 'gesund'
Names[2] = 'berufsunfaehig'


Arbeitsvertrag = c(3000, 3000, 0)
Erwerbsminderungsrente = c(915, 915)



for(i in 1:length(Names)){
  
  if (i==1){
    Name_frame[(i-1)*3+1] = Names[i]
    Einkommen_frame[(i-1)*3+1] = "Arbeitsvertrag"
    Wert_frame[(i-1)*3+1] = Arbeitsvertrag[i] * 12 * 34

    Name_frame[(i-1)*3+2] = Names[i]
    Einkommen_frame[(i-1)*3+2] = "Krankentagegeld"
    Wert_frame[(i-1)*3+2] = 0
    
    Name_frame[(i-1)*3+3] = Names[i]
    Einkommen_frame[(i-1)*3+3] = "Erwerbsminderungsrente"
    Wert_frame[(i-1)*3+3] = 0
    
  } else {
    Name_frame[(i-1)*3+1] = Names[i]
    Einkommen_frame[(i-1)*3+1] = "Arbeitsvertrag"
    Wert_frame[(i-1)*3+1] = 0
    
    Name_frame[(i-1)*3+2] = Names[i]
  Einkommen_frame[(i-1)*3+2] = "Krankentagegeld"
  Wert_frame[(i-1)*3+2] = Arbeitsvertrag[i] * 0.8 * 1.5 * 12
  
  Name_frame[(i-1)*3+3] = Names[i]
  Einkommen_frame[(i-1)*3+3] = "Erwerbsminderungsrente"
  Wert_frame[(i-1)*3+3] = Erwerbsminderungsrente[i] * 31 * 12
  }
}



phase = data.frame(Name_frame, Einkommen_frame, Wert_frame)
colnames(phase) = c('Name', 'Lebensabschnitt', 'Dauer')

n1 <- nPlot(Dauer ~ Name, data = phase, group = 'Lebensabschnitt', type = 'multiBarChart')
n1$yAxis(tickFormat = "#!function(d) {return (d/1000000).toFixed(2) + ' Mio';}!#")
n1$chart(width = 450) 

