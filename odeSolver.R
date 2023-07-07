library(deSolve)

model <- function(t, x, params) {
    # extract state variables
    x1 <- x[1]
    x2 <- x[2]
    # extract parameters
    k <- params["k"]
    # model equations
    dx1 <- x2
    dx2 <- -k * x1
    # combine results into a single vector
    dxdt <- c(dx1, dx2)
    # return result as a list
    list(dxdt)
}
