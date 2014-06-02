n <- 1000
scale <- runif(n,0,1)
data <- data.frame(A = floor(runif(n,1,4)),
                   B = floor(runif(n,1,3)*scale),
                   C = floor(runif(n,1,5)*scale))

