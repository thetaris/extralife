require(Rook)
require(brew)
require(shiny)

set.seed(Sys.time())
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
server$browse('peercheck')

