#' particle filter prediction
#'
#' @param x Distribution of random variable (fix to be more specific)
#' @param y Observed data
#' @param n Number of particles simulated
#' @param period The period using in time series prediction
#' @param pred.times The prediction range
#' @param filter Whether use particle filter or not
#' @return De-noised observations
#' @examples temp = sim_data(50,0.4,1.2)
#' particleFilter.Predcit(temp$x,temp$y,100,filter = TRUE)
#' particleFilter.Predcit(temp$x,temp$y,100,filter = FALSE)
#'
particleFilterPredcit = function(x,y,n,period=1,pred.times=10,filter=TRUE)
{
  library(forecast)

  if (filter == TRUE)
  {
    filter.mean = particleFilter(x,y,n)
    ts = ts(filter.mean,frequency  = period)
    model = auto.arima(ts)
    results = forecast(model, level=c(95), h=pred.times)
    plot(results)
    return(results)
  }
  if (filter == FALSE){

    ts = ts(y,frequency  = period)
    model = auto.arima(ts)
    results = forecast(model, level=c(95), h=pred.times)
    plot(results)
    return(results)
  }
}
