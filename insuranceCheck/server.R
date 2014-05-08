library(shiny)
library(rCharts)
source("../common/INIT.R", chdir=T, encoding="UTF-8")

source("computations.R", encoding="UTF-8")

formatDigits <- function(t) {
  html <- ""
  x <- '<img src= "euro.png"></img>'
  y <-paste(rep('<img src= "euro.png"></img>', 3), collapse="")
  rear <- paste(rep(y,floor(t/3)), collapse="<img src='dot.png'>")
  front <- paste(rep(x, t%%3), collapse="")
  if(front !="" && rear != "")
    html <- paste( front, rear, sep ="<img src='dot.png'>")
  else{
    if(rear !="")
      html <- rear
    if(front !="")
      html <- front
  }
  HTML(paste("<div class = 'thirdcolumn'>" , html , "</div>"))
  
}


shinyServer(function(input, output, session) {
  
  dataObj = isolate(DGSData(session=session))
  versicherungen <- getVersicherungen(dataObj)
  besitz <- getBesitz(dataObj)
  familie <- getFamilie(dataObj)
  recom <- getEmpfehlungen(versicherungen, besitz, familie, list("variable"="wenig"))
  
  
  output$main_plot <- renderTable({
    
    table.title <-  sapply(recom, function(rec) {paste0("<div class = 'secondcolumn'>", HTML(rec$titel), "</div>") })
    
    exposure <- sapply(recom, function(rec) {as.numeric(rec$schaden)})
    m <- sapply(exposure, formatDigits)
    
    table.status <- sapply(recom, function(rec) {   
      HTML(paste0(paste0("<div class=", rec$status, ">"), rec$absicherung, "</div>") )     
    })
    
    mydf <- data.frame(RISIKO = table.title,                      
                       
                       "MÖGLICHER SCHADEN" = m,               
                       
                       STATUS = table.status               
    )
  }, sanitize.text.function = function(x) x, include.rownames=FALSE)
  
  
  renderMyDataTable <- function(no) {
    if(nrow(recom[[no]]$vertraegeTabelle ) != 0 ){
      renderTable({ recom[[no]]$vertraegeTabelle }, sanitize.text.function = function(x) x, include.rownames=FALSE)
    }
   else{
     renderUI({HTML("<div class='nodata'> KEINE </div>")})
   }
  }
  
  renderMyempfehlungText <- function(no){
    renderUI({recom[[no]]$empfehlung})
  }
  
  renderMyweiteinfoText<- function(no){
    renderUI({recom[[no]]$infoHTML})
  } 
  output$mytable_privatehaftpflicht <- renderMyDataTable(1)
  output$mytable_krankheit <- renderMyDataTable(2)
  output$mytable_invaliditaet <- renderMyDataTable(3)
  output$mytable_tod <- renderMyDataTable(4)
  output$mytable_schadenamauto <- renderMyDataTable(5)
  output$mytable_kfz <- renderMyDataTable(6)
  output$mytable_schadenameigentum <- renderMyDataTable(7)
  output$mytable_rechtsstreit <- renderMyDataTable(8)
  
  output$RecomText_privatehaftpflicht <- renderMyempfehlungText(1)
  output$RecomText_krankheit <- renderMyempfehlungText(2)
  output$RecomText_invaliditaet <- renderMyempfehlungText(3)
  output$RecomText_tod  <- renderMyempfehlungText(4)
  output$RecomText_schadenamauto <- renderMyempfehlungText(5)
  output$RecomText_kfz <- renderMyempfehlungText(6)
  output$RecomText_schadenameigentum <- renderMyempfehlungText(7)
  output$RecomText_rechtsstreit <- renderMyempfehlungText(8)
  
  output$weiteinfoText_privatehaftpflicht <- renderMyweiteinfoText(1)
  output$weiteinfoText_krankheit <- renderMyweiteinfoText(2)
  output$weiteinfoText_invaliditaet <- renderMyweiteinfoText(3)
  output$weiteinfoText_tod <- renderMyweiteinfoText(4)
  output$weiteinfoText_schadenamauto <- renderMyweiteinfoText(5)
  output$weiteinfoText_kfz <- renderMyweiteinfoText(6)
  output$weiteinfoText_schadenameigentum <- renderMyweiteinfoText(7)
  output$weiteinfoText_rechtsstreit <- renderMyweiteinfoText(8)
  
  output$warning_privatehaftpflicht <- renderUI({
    div(class =paste0("personalWarning_", recom[[1]]$status), div(class = "warningpic"),
        div(class = "warningtext", div(class ="recomHeadline","ABSICHERUNG:"), div(class ="recomText",  recom[[1]]$absicherung ))
    )
  })
  output$warning_krankheit <- renderUI({
    div(class =paste0("personalWarning_", recom[[2]]$status), div(class = "warningpic"),
        div(class = "warningtext", div(class ="recomHeadline","ABSICHERUNG:"), div(class ="recomText",  recom[[2]]$absicherung ))
    )
  })
  output$warning_invaliditaet <- renderUI({
    div(class =paste0("personalWarning_", recom[[3]]$status), div(class = "warningpic"),
        div(class = "warningtext", div(class ="recomHeadline","ABSICHERUNG:"), div(class ="recomText",  recom[[3]]$absicherung ))
    )
  })
  output$warning_tod<- renderUI({
    div(class =paste0("personalWarning_", recom[[4]]$status), div(class = "warningpic"),
        div(class = "warningtext", div(class ="recomHeadline","ABSICHERUNG:"), div(class ="recomText",  recom[[4]]$absicherung ))
    )
  })
  output$warning_schadenamauto <- renderUI({
    div(class =paste0("personalWarning_", recom[[5]]$status), div(class = "warningpic"),
        div(class = "warningtext", div(class ="recomHeadline","ABSICHERUNG:"), div(class ="recomText",  recom[[5]]$absicherung ))
    )
  })
  output$warning_kfz <- renderUI({
    div(class =paste0("personalWarning_", recom[[6]]$status), div(class = "warningpic"),
        div(class = "warningtext", div(class ="recomHeadline","ABSICHERUNG:"), div(class ="recomText",  recom[[6]]$absicherung ))
    )
  })
  output$warning_schadenameigentum <- renderUI({
    div(class =paste0("personalWarning_", recom[[7]]$status), div(class = "warningpic"),
        div(class = "warningtext", div(class ="recomHeadline","ABSICHERUNG:"), div(class ="recomText",   recom[[7]]$absicherung))
    )
  })
  output$warning_rechtsstreit <- renderUI({
    div(class =paste0("personalWarning_", recom[[8]]$status), div(class = "warningpic"),
        div(class = "warningtext", div(class ="recomHeadline","ABSICHERUNG:"), div(class ="recomText",  recom[[8]]$absicherung))
    )
  })
})