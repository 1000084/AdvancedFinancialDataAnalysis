
computeReturnsFromXTS <- function(ts, col)
{
  ts.delta <- as.vector(diff(ts[,col])[-1])
  ts.rtn <- (ts.delta/as.vector(ts[-nrow(ts),col]))
  return (xts(ts.rtn, order.by=(index(ts)[-1])))
}

computeLogReturnsFromXTS <- function(ts, col)
{
  return (xts(diff(log(ts[,col]))[-1], order.by=(index(ts))[-1]))
}

getYahooData <- function(ticker, start, end, train.factor)
{
  daily.ret <- dailyReturn(na.omit(getSymbols(ticker, from=start, to=end, auto.assign = F)), type='log')
  forecast.origin <- floor(train.factor*length(daily.ret))
  ret.train <- daily.ret[1:forecast.origin,]
  ret.test <- daily.ret[(forecast.origin+1):length(daily.ret),]
  return (list(train=ret.train, test=ret.test))
}

getSPYReturns <- function(start, end, train.factor)
{
  return (getYahooData('SPY', start, end, train.factor=train.factor))
}

getSwissIndexReturns <- function(start, end, train.factor)
{
  return(getYahooData('^SSMI', start, end, train.factor=train.factor))
}

getKLCompositeIndexReturns <- function(start, end, train.factor)
{
  return(getYahooData('^KLSE', start, end, train.factor=train.factor)) 
}

getHangSengIndexReturns <- function(start, end, train.factor)
{
  return(getYahooData('^HSI', start, end, train.factor=train.factor)) 
}

getShanghaiCompositeIndexReturns <- function(start, end, train.factor)
{
  return(getYahooData('000001.SS', start, end, train.factor=train.factor)) 
}

getMSCIEmergingMarketsIndex <- function(start, end, train.factor)
{
  return(getYahooData('EEM', start, end, train.factor=train.factor)) 
}