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
  beta <- as.numeric(unlist(params["beta"]))
  k <- as.numeric(unlist(params["k"]))
  u <- as.numeric(unlist(params["u"]))
  tx <- as.numeric(unlist(params["tx"]))
  b <- as.numeric(unlist(params["b"]))
  
  dy <- (pi/12) * (xc + b)
  dxc <- (pi/12) * ((u * (xc - ((4*xc**3/2)))) - (y * ((24/(0.99669 * tx))**2 + (k*b))))
  dn <- 60 * (light(t, params) * (1 - n) - (beta * n))
  # combine results into a single vector
  #dxdt <- c(dx1, dx2)
  # return result as a list
  return(list(c(dy, dxc, dn)))
}

