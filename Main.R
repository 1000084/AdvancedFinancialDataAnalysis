foldername <- "/Users/swatimital/GitHub/TimeSeriesAnalysis/"
source(paste(foldername, "TimeSeriesData.r", sep=""))
source(paste(foldername, "StochasticVarFitting.r", sep=""))
source(paste(foldername, "StochasticVarForecasting.r", sep=""))
source(paste(foldername, "GarchFitting.r", sep=""))
source(paste(foldername, "GarchForecasting.r", sep=""))
source(paste(foldername, "EvaluationMeasures.r", sep=""))

end <- format(Sys.Date(), "%Y-%m-%d")
start <- format(Sys.Date() - 5*365, "%Y-%m-%d")

#end <- "2015-03-30"
#start <- "2012-01-01"

ret.dataset <- getMSCIEmergingMarketsIndex(start, end, 0.9)
plot(ret.dataset$train, main='Returns')

# Fit SV model
tryCatch({
  sv.fit <- fitStochasticVarianceModel(ret.dataset$train)
}, error = function(err) {
  stop(paste("ERROR: ", err))
})
annual.fitted.sv.vol <- getAnnualStochasticVol(sv.fit$fkf)[-1]
dates <- index(ret.dataset$train)[-1]
stochastic.vol <- xts(annual.fitted.sv.vol[-length(annual.fitted.sv.vol)], order.by=dates)

# Fit GARCH(1,1) mode
garch.fit <- fitGarch11Model(ret.dataset$train)
garch.vol <- getAnnualizedGarchVol(garch.fit, ret.dataset$train)

# Compute Realized Vol
realized.vol <- actualVolatility(ret.dataset$train)

# Plot fitted vols
plot(stochastic.vol, ylim=c(0.07, 0.6), xlab='Dates', ylab='Volatility', main='MSCI EM Fitted Vols')
lines(garch.vol, col='green')
lines(realized.vol, col='red')
legend('topright', c("SV", "GARCH", "Realized"), lty=1, col=c('black', 'red', 'green'))

garch11.aic <- aic(-garch.fit@fit$llh, length(ret.dataset$train), length(garch.fit@fit$par))
sv.aic <- aic(-sv.fit$fkf$logLik, length(ret.dataset$train), length(sv.fit$coeff$par))

garch11.bic <- bic(-garch.fit@fit$llh, length(ret.dataset$train), length(garch.fit@fit$par))
sv.bic <- bic(-sv.fit$fkf$logLik, length(ret.dataset$train), length(sv.fit$coeff$par))


forecast.horizon <- 60

#Forecast SV Model
stochastic.vol.forecast <- stochasticVolForecasts(sv.fit$coeff, ret.dataset$train, ret.dataset$test, horizon=forecast.horizon)
stochastic.vol.forecast <- xts(stochastic.vol.forecast, order.by=index(ret.dataset$test[1:forecast.horizon]))
plot(stochastic.vol.forecast, ylim=c(0.17, 0.33), xlab='Dates', ylab='Volatility', main='MSCI EM Forecast Vols')
# Plot Realized Vol
realized.vol.forecast <- actualVolatility(c(tail(ret.dataset$train, 19), ret.dataset$test[1:forecast.horizon]))
lines(realized.vol.forecast, col='red')
# Plot GARCH(1,1) vol
garch.forecast <- garch11VolForecasts(garch.fit, ret.dataset$train, ret.dataset$test, horizon=forecast.horizon)
garch.forecast <- xts(garch.forecast, order.by=index(ret.dataset$test[1:forecast.horizon]))
lines(garch.forecast, col='green')

legend('topright', c("SV", "GARCH", "Realized"), lty=1, col=c('black', 'red', 'green'))

# Combined plot
# Plot fitted vols
plot(c(stochastic.vol, stochastic.vol.forecast), ylim=c(0.07, 0.6), xlab='Dates', ylab='MSCI EM Volatility', main='Vols')
lines(c(garch.vol, garch.forecast), col='green')
lines(c(realized.vol, realized.vol.forecast), col='red')
legend('topright', c("SV", "GARCH", "Realized"), lty=1, col=c('black', 'red', 'green'))

sv.mae <- meanAbsoluteError(stochastic.vol.forecast^2, ret.dataset$test[1:forecast.horizon])
garch.mae <- meanAbsoluteError(garch.forecast^2, ret.dataset$test[1:forecast.horizon])

sv.mse <- meanSquareError(stochastic.vol.forecast^2, ret.dataset$test[1:forecast.horizon])
garch.mse <- meanSquareError(garch.forecast^2, ret.dataset$test[1:forecast.horizon])

sv.rmse <- rootMeanSquareError(stochastic.vol.forecast^2, ret.dataset$test[1:forecast.horizon])
garch.rmse <- rootMeanSquareError(garch.forecast^2, ret.dataset$test[1:forecast.horizon])

sv.mae
garch.mae
sv.mse
garch.mse
sv.rmse
garch.rmse


sv.aic
garch11.aic

sv.bic
garch11.bic

