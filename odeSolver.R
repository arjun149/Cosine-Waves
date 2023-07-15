library(deSolve)

model <- function(t, x, params) {
  # extract state variables
  x1 <- x[1] #x
  x2 <- x[2] #xc
  # extract parameters
  #params["k", "u", "tx", "b"]
  
  # model equations
  k <- params["k"]
  u <- params["u"]
  tx <- params["tx"]
  b <- params["b"]
  
  dx1dt <- (pi/12) * (x2 + b)
  dx2dt <- (pi/12) * ((u * (x2 - ((4*x^3)/2))) - (x1 * ((24/(0.99669 * tx))^2 + (k*b))))
  # combine results into a single vector
  #dxdt <- c(dx1, dx2)
  # return result as a list
  return(list(c(dx1dt, dx2dt)))
}

