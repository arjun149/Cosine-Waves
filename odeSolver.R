library(deSolve)



light <- function(t, params) {
  lux <- params["maxLux"]
  lights_on <- params["lights_on"]
  t_periodic <- t %% 24
  if (as.numeric(t_periodic) < as.numeric(lights_on)) {
    return (0.0)
  } else {
    return (lux)
  }
}


model <- function(t, x, params) {
  # extract state variables
  y <- x[1] #x
  xc <- x[2] #xc
  n <- x[3]
  # extract parameters
  #params["k", "u", "tx", "b"]
  
  # model equations
  beta <- params["beta"]
  k <- params["k"]
  u <- params["u"]
  tx <- params["tx"]
  b <- params["b"]
  
  dy <- as.numeric((pi/12) * (xc + b))
  dxc <- as.numeric((pi/12) * ((u * (xc - ((4*xc**3/2)))) - (y * ((24/(0.99669 * tx))**2 + (k*b)))))
  dn <- as.numeric(60 * (light(t, params) * (1 - n) - (beta * n)))
  # all derivatives with respect to t
  # combine results into a single vector
  #dxdt <- c(dx1, dx2)
  # return result as a list
  return(list(c(dy, dxc, dn)))
}

