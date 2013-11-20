# install_github('rCharts', 'ramnathv', ref = 'dev') 
# require(rCharts)
Versicherung = c('private Haftpflicht',
'Tod (Homer)',
'Tod (Marge)',                 
'Beruftsunf&auml;higkeit (Homer)',
                 'Unfall (Lisa)',
                 'Unfall (Bart)',
                 'Unfall (Maggy)',
                 'Unfall (Marge)',
                 'Hausrat',
                 'Kranken',
                 'Wohngeb&auml;ude',
                 'Pflegezusatz',
                 'Haus- und Grundbesitzerhaftpflicht',
                 'Rechtschutz'
                 )

Abdeckung = c('<a href = "test">gut</a>',
'keine',
'keine',
'gesetzlich',
'keine',
'keine',
'keine',
'keine',
'keine',
'gesetzlich',
'-',
  '-',
  '-',
  'keine')

Status = c(1,2,2,2,2,2,2,1,0,1,0,2,0,0)

Wichtigkeit = c(
'<img src="C:/Users/master/Pictures/DieGraueSeite/signal_0.png" width="25"/>',
  '+++',
'+++',
  '+++',
  '++',
  '++',
  '++',
  '++',
  '++',
  '+++',
  '+++',
  '++',
  '+',
  '+')

Angebote = c('-',
               '17 &euro;',
             '15 &euro;',
             '60 &euro;',
             '8 &euro;',
             '8 &euro;',
             '8 &euro;',
             '-',
               '5 &euro;',
             '-',
               '-',
               '22 &euro;',
             '-',
               '-')
  
myframe = data.frame(Versicherung, Abdeckung, Status, Wichtigkeit, Angebote)

dt <- dTable
dt <- dTable(myframe, sPaginationType=  "full_numbers")


Deine_wichtigsten_Risiken = c('<a href="krankheit">Krankheit</a>',
                              '<a href="invaliditaet">Invalidit&auml;t</a>',
                              '<a href="tod">Tod</a>',
                              '<a href="autoschaden">Schaden am Auto</a>',
                              '<a href="haftpflicht">private Haftpflicht</a>',
                              '<a href="haftpflichtprivat">KFZ Haftpflicht</a>',
                              '<a href="eigentumsschaden">Schaden am Eigentum</a>',
                              '<a href="rechtsstreit">Rechtsstreit</a>'
)


Geldbedarf_bei_Schaden = c('&euro;.&euro;&euro;&euro;.&euro;&euro;&euro;',
                           '&euro;.&euro;&euro;&euro;.&euro;&euro;&euro;',
                           '&euro;&euro;&euro;.&euro;&euro;&euro;',
                           '&euro;&euro;.&euro;&euro;&euro;',
                           '&euro;.&euro;&euro;&euro;.&euro;&euro;&euro;',
                           '&euro;&euro;.&euro;&euro;&euro;.&euro;&euro;&euro;',
                           '&euro;&euro;.&euro;&euro;&euro;',
                           '&euro;&euro;.&euro;&euro;&euro;'
                           )

Abdeckung = c('<img src="C:/Users/master/Pictures/DieGraueSeite/Signal_5.png" width="25"/>',
              '<img src="C:/Users/master/Pictures/DieGraueSeite/Signal_2.png" width="25"/>',
              '<img src="C:/Users/master/Pictures/DieGraueSeite/Signal_1.png" width="25"/>',
              '<img src="C:/Users/master/Pictures/DieGraueSeite/Signal_3.png" width="25"/>',
              '<img src="C:/Users/master/Pictures/DieGraueSeite/Signal_5.png" width="25"/>',
              '<img src="C:/Users/master/Pictures/DieGraueSeite/Signal_5.png" width="25"/>',
              '<img src="C:/Users/master/Pictures/DieGraueSeite/Signal_0.png" width="25"/>',
              '<img src="C:/Users/master/Pictures/DieGraueSeite/Signal_0.png" width="25"/>'
)

Status = c('<div style="display:none">1</div> <img src="C:/Users/master/Pictures/DieGraueSeite/Check_Icon_32.png"/>',
           '<div style="display:none">0</div> <img src="C:/Users/master/Pictures/DieGraueSeite/Warning_Icon_32.png"/>',
           '<div style="display:none">0</div> <img src="C:/Users/master/Pictures/DieGraueSeite/Warning_Icon_32.png"/>',
           '<div style="display:none">1</div> <img src="C:/Users/master/Pictures/DieGraueSeite/Check_Icon_32.png"/>',
           '<div style="display:none">1</div> <img src="C:/Users/master/Pictures/DieGraueSeite/Check_Icon_32.png"/>',
           '<div style="display:none">0</div> <img src="C:/Users/master/Pictures/DieGraueSeite/Warning_Icon_32.png"/>',           
           '<div style="display:none">1</div> <img src="C:/Users/master/Pictures/DieGraueSeite/Check_Icon_32.png"/>',
           '<div style="display:none">1</div> <img src="C:/Users/master/Pictures/DieGraueSeite/Check_Icon_32.png"/>'
           )

myframe = data.frame(Deine_wichtigsten_Risiken, Geldbedarf_bei_Schaden, Abdeckung, Status)

dt <- dTable
dt <- dTable(myframe, sPaginationType=  "full_numbers")
