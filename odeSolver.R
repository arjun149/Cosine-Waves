library(deSolve)



alpha <- function(i, params) {
  i <- params["i"]
  # i0 = 9500
  # a0 = 0.16
  return (0.16 * (as.numeric(i) / 9500))
}

model <- function(t, x, params) {
  # extract state variables
  y <- x[1] #x
  xc <- x[2] #xc
  n <- x[3]
  # extract parameters
  #params["k", "u", "tx", "b"]
  
  # model equations
  i<- params["i"]
  beta <- params["beta"]
  k <- params["k"]
  u <- params["u"]
  tx <- params["tx"]
  b <- params["b"]
  
  dy <- as.numeric((pi/12) * (xc + b))
  dxc <- as.numeric((pi/12) * ((u * (xc - ((4*xc**3/2)))) - (y * ((24/(0.99669 * tx))**2 + (k*b)))))
  dn <- as.numeric(60 * (alpha(i, params) * (1 - n) - (beta * n)))
  # all derivatives with respect to t
  # combine results into a single vector
  #dxdt <- c(dx1, dx2)
  # return result as a list
  return(list(c(dy, dxc, dn)))
}
