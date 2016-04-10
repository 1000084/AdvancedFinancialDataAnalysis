library(fGarch)
library(fArma)

# Forecast 1 time step volatility
oneStepAheadGarch11Forecasting <- function(fit, r.t, sigma.t) 
{
  coeff <- fit@fit$matcoef
  garch.coeff <- c(coeff["omega",1], coeff["alpha1",1], coeff["beta1",1])
  return (sqrt(sum(garch.coeff * c(1,  r.t^2, sigma.t^2) )))
}

garch11VolForecasts <- function(fit, train.data, test.data, horizon=30)
{
  garch.sigma.forecast <- c()
  last.ret <- tail(train.data, 1)
  last.sigma <- tail(fit@sigma.t, 1)
  
  for (i in 1:horizon)
  {
    garch.sigma.1ahead <- oneStepAheadGarch11Forecasting(fit, last.ret, last.sigma)
    garch.sigma.forecast <- c(garch.sigma.forecast, garch.sigma.1ahead)
    last.sigma <- garch.sigma.1ahead
    last.ret <- test.data[i]
  }
  
  return (garch.sigma.forecast * sqrt(252))
}

testFunction <- function()
{
  forecast.horizon <- 10
  forecast.steps <- c(1,7,14,21,30)
  forecast.sigma <- matrix(0,nrow=forecast.horizon,ncol=length(forecast.steps))
  colors <- sample(colours(), forecast.horizon)
  
  for (i in 1:forecast.horizon)
  {
    garch.fit <- garchFit(formula=~garch(1,1), data=returns, trace=F, cond.dist='std', include.mean = F)
    #plot(xts(sqrt(252) * garch.fit@sigma.t, order.by=index(spy.ret.train)))
    sigma.predict.1day <- garchForecastVolatility1Day(garch.fit, tail(returns,1), tail(garch.fit@h.t,1))
    sigma.predict <- garchForecastVolatility(garch.fit, forecast.steps[-1])
    forecast.sigma[i,] <- c(sigma.predict.1day, sigma.predict)
    returns <- c(returns, as.vector(spy.ret.test[i,1]))
    
    if (i == 1) {
      plot(i:(i+length(forecast.steps)-1), forecast.sigma[i,], col=colors[i], type='l', xlim=c(1,forecast.horizon+length(forecast.steps)+1))
    } else {
      lines(i:(i+length(forecast.steps)-1), forecast.sigma[i,], col=colors[i])
    }
  }
  
  plot(1:forecast.horizon, forecast.sigma[,1], col='red', type='l')
}

