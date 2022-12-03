#' Simulate the hidden variables X and observable variables Y for particle filters.
#' The observable variables (observation process) are related to
#' the hidden variables (state-process) by gaussian distribution in this simulation.
#'
#' @param time The number of observations for data simulated
#' @param noise The noise of X
#' @param relation The relation of X
#' @return A list of simulated X and Y vector, each with `length = time`
#' @examples
#' sim_data(50,0.4,1.2)
#' x_true = sim_data(50,0.4,1.2)$x
#' obs = sim_data(50,0.4,1.2)$y
sim_data= function(time, noise, relation)
{

  x = rep(NA, time)
  y = rep(NA, time)

  # set the initial values
  x[1] = rnorm(1, 0, 1)
  y[1] = rnorm(1, x[1], relation)

  # simulate the values
  for (i in 2:time) {
    x[i] = rnorm(1, x[i-1], noise)
    y[i] = rnorm(1, x[i], relation)
  }
  return(list(x = x, y = y))
}
