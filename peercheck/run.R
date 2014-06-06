require(Rook)
require(brew)

source('questions.R', encoding='UTF-8')
source('access.R', encoding='UTF-8')

server <- Rhttpd$new()
server$start(quiet=TRUE)

server$add(name="peercheck",
      app=Builder$new(
        Brewery$new(url="/", root="www"),
        Redirect$new("/welcome.html")
      )
)
server$add(app = File$new("plots"), name = "plots")

server$browse('peercheck')


