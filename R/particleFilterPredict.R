#' particle filter prediction
#'
#' @param x The hidden state variables
#' @param y The observation variables
#' @param nParticle The number of particles simulated
#' @param period The period using in time series prediction
#' @param pred.n The prediction range
#' @param filter The option of whether use particle filter or not
#' @return The prediction of observations
#' @examples
#' temp = sim_data(50,0.4,1.2)
#' particleFilterPredict(temp$x,temp$y,100,filter = TRUE)
#' particleFilterPredict(temp$x,temp$y,100,filter = FALSE)
particleFilterPredict = function(x,y,nParticle,period=1,pred.n=10,filter=TRUE)
{
  if (filter == TRUE)
  {
    filter.mean = particleFilter(x,y,nParticle)
    ts = ts(filter.mean,frequency  = period)
    model = auto.arima(ts)
    results = forecast(model, level=c(95), h=pred.n)
    plot(results)
    return(results)
  }
  if (filter == FALSE){

    ts = ts(y,frequency  = period)
    model = auto.arima(ts)
    results = forecast(model, level=c(95), h=pred.n)
    plot(results)
    return(results)
  }
}
