library(quantmod)
library(fGarch)

fitGarch11Model <- function(returns)
{
  rets <- as.vector(returns)
  garch.fit <- garchFit(formula=~garch(1,1), data=returns, trace=F, cond.dist='std', include.mean = F)
  return (garch.fit)
}

getAnnualizedGarchVol <- function(fit, rets)
{
  return (xts(sqrt(252) * fit@sigma.t, order.by=index(rets)))
}