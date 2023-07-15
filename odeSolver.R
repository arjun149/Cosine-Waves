library(deSolve)

model <- function(t, x, params) {
    # extract state variables
    x1 <- x[1] #x
    x2 <- x[2] #xc
    # extract parameters
    k, u, tx, b <- params["k", "u", "tx", "b"]
    # model equations
    dx1 <- (pi/12) * (x2 + b)
    dx2 <- (pi/12) * ((u * (x2 - ((4*x^3)/2))) - (x1 * ((24/(0.99669 * tx))^2 + (k*b))))
    # combine results into a single vector
    dxdt <- c(dx1, dx2)
    # return result as a list
    list(dxdt)
}
