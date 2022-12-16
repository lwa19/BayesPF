#' Simulate the hidden variables X and observable variables Y for particle filters.
#' The observable variables (observation process) are related to
#' the hidden variables (state-process) by beta distribution in this simulation.
#' We will assume that in each state there are a finite number of "counts" of
#' successes and failures. The successes are modeled with a Poisson distribution
#' with rate "success_rate".
#'
#' @param time The number of observations for data simulated
#' @param trials The number of trials
#' @param success_rate The rate of success in the underlying process
#' @param relation The relation of X and Y
#' @return A list of simulated X and Y vector, each with `length = time`
#' @examples
#' sims = sim_data(100,10,0.2, 0.2)
#' x_true = sims$x
#' obs = sims$y
sim_data_counts= function(time, trials, success_rate, relation)
{
  x = rep(NA, time)
  y = rep(NA, time)

  # set the initial values
  x[1] = rbinom(1, trials, success_rate)
  y[1] = rbeta(1, x[1] + 1, trials - x[1] + 1) + abs(rnorm(1, 0, relation))

  # simulate the values
  for (i in 2:time) {
    x[i] = rbinom(1, trials, x[i-1]/trials)
    y[i] = rbeta(1, x[i] + 1, trials - x[i] + 1) + abs(rnorm(1, 0, relation))
  }
  return(list(x = x, y = round(y * trials)))
}
