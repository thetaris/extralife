library(shiny)


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # The following is done once per session:
  
  
  
  # The following is done on change of input

  
  output$myChart <- renderChart2({
    
    #print(data)
    
    # read data and hold in session
    
    # grab the sessionId
    
    # plot einkommensausfall
    ##################################################################################################
    
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
    
    Abdeckung = c('<img src="http://thetava.com/Shiny_icons/Signal_5.png" width="25"/>',
                  '<img src="http://thetava.com/Shiny_icons/Signal_2.png" width="25"/>',
                  '<img src="http://thetava.com/Shiny_icons/Signal_1.png" width="25"/>',
                  '<img src="http://thetava.com/Shiny_icons/Signal_3.png" width="25"/>',
                  '<img src="http://thetava.com/Shiny_icons/Signal_5.png" width="25"/>',
                  '<img src="http://thetava.com/Shiny_icons/Signal_5.png" width="25"/>',
                  '<img src="http://thetava.com/Shiny_icons/Signal_0.png" width="25"/>',
                  '<img src="http://thetava.com/Shiny_icons/Signal_0.png" width="25"/>'
    )
    
    Status = c('<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>',
               '<div style="display:none">0</div> <img src="http://thetava.com/Shiny_icons/Warning_Icon_32.png"/>',
               '<div style="display:none">0</div> <img src="http://thetava.com/Shiny_icons/Warning_Icon_32.png"/>',
               '<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>',
               '<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>',
               '<div style="display:none">0</div> <img src="http://thetava.com/Shiny_icons/Warning_Icon_32.png"/>',           
               '<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>',
               '<div style="display:none">1</div> <img src="http://thetava.com/Shiny_icons/Check_Icon_32.png"/>'
    )
    
    myframe = data.frame(Deine_wichtigsten_Risiken, Geldbedarf_bei_Schaden, Abdeckung, Status)
    
    
    dTable(myframe, sPaginationType=  "full_numbers")
    
    #dt$addParams(dom = 'myChart')
    #return(dt)
    
    ##################################################################################################
    
  })
  
  
  
})