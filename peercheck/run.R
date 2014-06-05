require(Rook)
require(brew)

server <- Rhttpd$new()
server$start(quiet=TRUE, port=9010)

server$add(name="peercheck",
      app=Builder$new(
        Brewery$new(url="/",root="www"),
        Redirect$new("/welcome.html")
      )
)
server$add(app = File$new("plots"), name = "plots")

server$browse('peercheck')


