require(Rook)
require(brew)
require(shiny)

set.seed(Sys.time())
if (file.exists('peercheck')) setwd("peercheck")
source('questions.R', encoding='UTF-8')
source('access.R', encoding='UTF-8')
source('format.R', encoding='UTF-8')

server <- Rhttpd$new()

server$add(name="peercheck",
           app=Builder$new(
             Brewery$new(url="/", root="www"),
             Redirect$new("/welcome.html")
           )
)
server$add(app = File$new("style"), name = "style")
server$add(app = File$new("plot"), name = "plot")

server$add(name="plots", function(env) {
  request <- Request$new(env)
  response <- Response$new()
  
  path <<- request$path()
  questionid <- unlist(strsplit(path, "/"))[4]

  content <- getSatisfactionHTML(questionid, NULL)
  content <- paste0('<head><meta content="text/html" charset="UTF-8"></head>', content)
  
  response$write(content)
  #response$write(paste("<img src =", server$full_url("plot"), "/example.png", ">", sep = ""))
  
  response$finish()
})
server$start(quiet=TRUE, port=9010)
server$browse('peercheck')


