require(Rook)
require(brew)
require(shiny)

source('questions.R', encoding='UTF-8')
source('access.R', encoding='UTF-8')
source('format.R', encoding='UTF-8')

server <- Rhttpd$new()

start <- function(){
  server$add(name="peercheck",
             app=Builder$new(
               Brewery$new(url="/", root="www", scheme='https'),
               Redirect$new("/welcome.html")
             )
  )
  server$add(app = File$new("style"), name = "style")
  server$start(quiet=TRUE, port=9010)
}

start()
server$browse('peercheck')

while (T) {
  Sys.sleep(5);
  start()
}

