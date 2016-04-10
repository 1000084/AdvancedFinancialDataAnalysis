library(fGarch)
library(fArma)

# Forecast 1 time step volatility
garchForecastVolatility1Day <- function(fit, r.t, sigma.t) 
{
  coeff <- fit@fit$matcoef
  garch.coeff <- c(coeff["omega",1], coeff["alpha1",1], coeff["beta1",1])
  return (sqrt(sum(garch.coeff * c(1,  r.t^2, sigma.t^2) )))
}

# Forecast volatility for multiple forecast steps using long term variance
garchForecastVolatility <- function(fit, forecast.steps)
{
  sigma.t <- tail(fit@h.t,1)
  coeff <- fit@fit$matcoef
  garch.coeff <- c(coeff["omega",1], coeff["alpha1",1], coeff["beta1",1])
  #long term variance
  lt.var <- garch.coeff[1]/(1-garch.coeff[2]-garch.coeff[3])
  sigma.predict <- unlist(lapply(forecast.steps, function(n) { sqrt(lt.var + (garch.coeff[2] + garch.coeff[3])^n * (sigma.t^2 - lt.var))}))
  return (sigma.predict)
}

garchFitPredictMultipleDays2 <- function(rets, n)
{
  history <- 500
  curr.t <- length(rets)
  next.t <- curr.t + 1
  rets.t <- as.vector(rets[(next.t - history):curr.t,])
  sigma.t <- c()
  
  #mm <- armaSearch(rets.t)
  #q <- mm$model[2]
  #p <- mm$model[1]
  
  for (i in 1:10)
  {
    #arma.fit <- armaFit(formula=~arma(5,3), data=rets.t)
    garch.fit <- garchFit(formula=~arma(2,1)+garch(1,1), data=rets.t, trace=F, cond.dist='std', include.mean = F)
    
    #a.t <- predict(arma.fit, 2)$pred
    a.t <- predict(garch.fit, 1)$meanForecast
    sigma.t <- c(sigma.t, garchFitPredict1Day(fit, tail(rets.t,1), tail(fit@h.t,1)))
    rets.t <- c(rets.t,a.t[1])
  }
  
  ts.plot(rets.t)
  ts.plot(sigma.t)
  return (sigma.predict)  
}

armaSearch = function(
  xx,  
  minOrder=c(0,0),
  maxOrder=c(5,5),
  trace=FALSE )
{
  bestAic = 1e9
  len = NROW( xx ) 
  for( p in minOrder[1]:maxOrder[1] ) for( q in minOrder[2]:maxOrder[2] )
  {
    if( p == 0 && q == 0 )
    {    
      next
    }    
    
    formula = as.formula( paste( sep="", "xx ~ arma(", p, ",", q, ")" ) )
    
    fit = tryCatch( armaFit( formula, data=xx ),
                    error=function( err ) FALSE,
                    warning=function( warn ) FALSE )
    if( !is.logical( fit ) )
    {    
      fitAic = fit@fit$aic
      if( fitAic < bestAic )
      {    
        bestAic = fitAic
        bestFit = fit
        bestModel = c( p, q )
      }    
      
      if( trace )
      {    
        ss = paste( sep="", "(", p, ",", q, "): AIC = ", fitAic )
        print( ss ) 
      }    
    }    
    else
    {    
      if( trace )
      {    
        ss = paste( sep="", "(", p, ",", q, "): None" )
        print( ss ) 
      }    
    }    
  }
  
  if( bestAic < 1e9 )
  {
    return( list( aic=bestAic, fit=bestFit, model=bestModel ) )
  }
  
  return( FALSE )
}

