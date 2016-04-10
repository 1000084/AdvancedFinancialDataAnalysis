meanSquareError <- function(estimate, returns)
{
  r.bar <- mean(returns)
  return(mean(((returns-r.bar)^2-estimate))^2)
}

rootMeanSquareError <- function(estimate, returns)
{
  return(sqrt(meanSquareError(estimate, returns)))
}

meanAbsoluteError <- function(estimate, returns)
{
  r.bar <- mean(returns)
  return(mean(abs((returns-r.bar)^2-estimate)))
}

actualVolatility <- function(returns, lookback=20)
{
  return(xts(apply(returns,2,runSD,n=lookback), index(returns))*sqrt(252))
}

aic <- function(maxLogLik, nData, nCoeffs)
{
  return (((-2*maxLogLik)/nData) + (2*nCoeffs/nData))
}

bic <- function(maxLogLik, nData, nCoeffs)
{
  return (((-2*maxLogLik)/nData) + (log(nData)*nCoeffs/nData))
}

