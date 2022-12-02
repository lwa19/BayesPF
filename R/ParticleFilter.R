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

#############################################################
x_true = sim_data(50,0.4,1.2)$x
obs = sim_data(50,0.4,1.2)$y

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

estimates_means = particleFilter(x_true,obs,1000)

plot(x =1:50,y=estimates_means,type="l",col="red")
lines(obs,col="blue")



################################# Compare to pomp
library(pomp)
temp = gompertz()
data1= as(temp,"data.frame")

estimates_means = particleFilter(data1[,3],data1[,2],1000)

plot(x =data1[,1],y=estimates_means,type="l",col="red")
lines(data1[,2],col="blue")

pf <- pfilter(temp,Np=1000,filter.mean=TRUE)
fm <- filter.mean(pf)


lines(as.vector(fm),col="black")
