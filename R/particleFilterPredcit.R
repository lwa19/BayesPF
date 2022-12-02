# modify period to have better precision of prediction
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


temp = sim_data(50,0.4,1.2)

particleFilter.Predcit(temp$x,temp$y,100,filter = TRUE)
particleFilter.Predcit(temp$x,temp$y,100,filter = FALSE)
