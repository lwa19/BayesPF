#' The particle filter
#' (The sequential Monte Carlo with multinomial importance resampling (SIR))
#'
#' @param x The hidden state variables
#' @param y The observation variables
#' @param nParticle The number of particles simulated
#' @return The de-noised observations
#' @examples
#' x_true = sim_data(50,0.4,1.2)$x
#' obs = sim_data(50,0.4,1.2)$y
#' estimates_means = particleFilter(x_true,obs,1000)
#' plot(x =1:50,y=estimates_means,type="l",col="red")
#' lines(obs,col="blue")
particleFilter = function(x, y, nParticle)
{
  time = length(y)
  estimates = matrix(NA, nrow =  nParticle, ncol = time)
  weights = matrix(NA, nrow =  nParticle, ncol = time)

  noise = sqrt(var(x))
  relation = sqrt(var(y))

  estimates[, 1] = y[1]
  weights[, 1] = dnorm(y[1], estimates[, 1], relation)
  weights[, 1] = weights[, 1]/sum(weights[, 1])

  estimates[, 1] = sample(estimates[, 1], replace = TRUE,
                          size = nParticle, prob = weights[, 1])

  for (i in 2:time) {
    estimates[, i] = rnorm(nParticle, estimates[, i-1], noise)
    weights[, i] = dnorm(y[i], estimates[, i], relation)
    weights[, i] = weights[, i]/sum(weights[, i])

    # resampling
    estimates[, i] = sample(estimates[, i], replace = TRUE,
                            size = nParticle, prob = weights[, i])
  }

  estimates_means = apply(estimates, 2, mean)
  return(estimates_means)
}

