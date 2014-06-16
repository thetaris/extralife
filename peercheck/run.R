require(Rook)
require(brew)
require(shiny)

if (file.exists('peercheck')) setwd("peercheck")
source('questions.R', encoding='UTF-8')
source('access.R', encoding='UTF-8')
source('format.R', encoding='UTF-8')

server <- Rhttpd$new()

deploy <- function(){
  server$add(name="peercheck",
             app=Builder$new(
               Brewery$new(url="/", root="www"),
               Redirect$new("/welcome.html")
             )
  )
  server$add(app = File$new("style"), name = "style")
}

deploy()
server$start(quiet=TRUE, port=9010)

if (Sys.info()['nodename']=='diegraueseite.de') {
  cat('Started in server mode. Restarts every 5 seconds.')
  while (T) {
    Sys.sleep(5);
    deploy()
  }
} else {
  server$browse('peercheck')
}

