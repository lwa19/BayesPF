# BayesPF
Bayesian Particle Filter implemented in R

## Motivation
The goal of making this package is to achieve the functionality of particle filter in R. The particle filter in this package is a simple Particle filter model based on Gaussian distribution. The particle filter could de-noise the observations in some specific pattern of the internal state variables x and partial observations y.

## Installation
```
# it is highly recommended that install the devtools if not have one
install.packages('devtools')

devtools::install_github("lwa19/BayesPF", build_vignettes = T)
library("BayesPF")
```
There are three functions in `BayesPF` package which are `sim_data`, `particleFilter`, and `particleFilterPredict` functions. 

## Features
The particle filter is designed for a hidden Markov Model, where the system made of both hidden and observable variables. The `BayesPF` is to achieve a basic functionality of particle filter. 

The `BayesPF` contains three functions. 
`sim_data` could simulate a set of simple data includes hidden states x and observations y given noise and relation of x. 
`particleFilter` uses a simple particle filter to de-noise the observations y based on x. 
`particleFilterPredict` could predict observations y based on time series analysis. We could choose `filter = TRUE` to turn on the particle filter in the prediction.

There is one R package named `pomp` having a function named `pfilter` which could give similar results to our particle filter. However, it can only use the data generated by `gompertz()` and it is relatively slow. Our package could use any equal length x and y variables and generate particle filter results ("filter mean"") with a better performance.


## Examples

Generating the data;
```
x_noise = 0.4
x_relation = 1.2
tempdata = sim_data(50,x_noise,x_relation)

tempdata
tempdata$x
tempdata$y
```

Using the particle filter to de-noise the observations y;
```
temp = sim_data(50,0.4,1.2)

particleFilter.Predcit(temp$x,temp$y,100,filter = TRUE)
particleFilter.Predcit(temp$x,temp$y,100,filter = FALSE)
```


Compared to `pfilter` in `library(pomp)` in accuracy;
```
library(pomp)
temp = gompertz()
data1= as(temp,"data.frame")
estimates_means = particleFilter(data1[,3],data1[,2],1000)
plot(x =data1[,1],y=estimates_means,type="l",col="red")
lines(data1[,2],col="blue")
pf <- pfilter(temp,Np=1000,filter.mean=TRUE)
fm <- filter.mean(pf)
lines(as.vector(fm),col="black")
```


Compared to `pfilter` in `library(pomp)` in efficiency;
```

```

More specific examples can be viewed at `browseVignettes("BayesPF")`.

