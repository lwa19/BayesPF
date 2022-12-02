#' Run the naive particle filter
#'
#' @param x Distribution of random variable (fix to be more specific)
#' @param y Observed data
#' @param n Number of particles simulated
#' @return De-noised observations
#' @examples
#' x_true = sim_data(50,0.4,1.2)$x
#' obs = sim_data(50,0.4,1.2)$y
#' estimates_means = particleFilter(x_true,obs,1000)
#' plot(x =1:50,y=estimates_means,type="l",col="red")
#' lines(obs,col="blue")


#############################################################
# particle filter
# sequential Monte Carlo with multinomial importance resampling (SIR)
# one dimension

# x - data of x dimension (1 * time)
# y - data of y dimension (1 * time)
# n - number of particles
particleFilter = function(x,y,n)
{
  time = length(y)
  estimates = matrix(NA, nrow =  n, ncol = time)
  weights = matrix(NA, nrow =  n, ncol = time)

  sx = sqrt(var(x))
  sy = sqrt(var(y))

  estimates[, 1] = y[1]
  weights[, 1] = dnorm(y[1], estimates[, 1], sy)
  weights[, 1] = weights[, 1]/sum(weights[, 1])
  #print(weights[,1])
  estimates[, 1] = sample(estimates[, 1], replace = TRUE,
                          size = n, prob = weights[, 1])

  for (i in 2:time) {
    estimates[, i] = rnorm(n, estimates[, i-1], sx)
    weights[, i] = dnorm(y[i], estimates[, i], sy)
    weights[, i] = weights[, i]/sum(weights[, i])

    # resampling
    estimates[, i] = sample(estimates[, i], replace = TRUE,
                            size = n, prob = weights[, i])
  }

  estimates_means = apply(estimates, 2, mean)
  return(estimates_means)
}

