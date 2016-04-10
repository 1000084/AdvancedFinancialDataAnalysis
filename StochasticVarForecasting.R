
oneStepAheadStochasticVolForecasting <- function(fit, historical.rets, last.ret)
{
  yt <- log(c(historical.rets, last.ret)^2)
  yt <- apply(yt, 1, function(x) ifelse(is.finite(x), x, NA))
  ytm <- matrix(as.vector(yt),nrow=1)
  
  sp <- stochasticVar(fit$par["gamma"], fit$par["mu"], fit$par["phi"], fit$par["sigma"])
  ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt, Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = ytm)
  
  return (tail(getAnnualStochasticVol(ans), 1))
}

stochasticVolForecasts <- function(fit, train.data, test.data, horizon=30)
{
  historical.rets <- ret.dataset$train[1:(length(train.data)-1)]
  last.ret <- tail(train.data,1)
  sv.sigma.forecast <- c()

  for (i in 1:horizon)
  {
    sv.sigma.forecast <- c(sv.sigma.forecast, 
                           oneStepAheadStochasticVolForecasting(fit, historical.rets, last.ret))
    historical.rets <- c(historical.rets, last.ret)
    last.ret <- test.data[i]
  }
  
  return (sv.sigma.forecast)
}