#' Simulate X and Y data
#'
#' @param time Number of observations for data simulated
#' @param xs Variance of simulated random samples
#' @param ys Variance of simulated observations
#' @return A list of simulated X and Y vector, each with `length = time`
#' @examples
#' x_true = sim_data(50,0.4,1.2)$x
#' obs = sim_data(50,0.4,1.2)$y


sim_data= function(time, sx, sy)
{

  x = rep(NA, time)
  y = rep(NA, time)

  # set the initial values
  x[1] = rnorm(1, 0, 1)
  y[1] = rnorm(1, x[1], sy)

  # simulate the values
  for (i in 2:time) {
    x[i] = rnorm(1, x[i-1], sx)
    y[i] = rnorm(1, x[i], sy)
  }
  return(list(x = x, y = y))
}
