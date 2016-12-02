library(forecast)

regression.arma <- function(y, w, h = 7){
    y.train <- window(y, end = length(y) - h ) 
    y.test <- window(y, start = length(y) - h + 1)
    w.train <- window(w, end = length(w) - h ) 
    w.test <- window(w, start = length(w) - h + 1)
    BC.lambda <- BoxCox.lambda(y.train)
    arima.mod.tran <- auto.arima(y.train, xreg = w.train, lambda = BC.lambda)
    arima.mod.tran.f <- forecast(arima.mod.tran, h = 7, xreg = w.test)
    preds <- arima.mod.tran.f$mean
    rmse <- sqrt( sum( (preds - as.numeric(y.test))^2 ) / ( length(y.test)) )
    return( list(rmse = rmse, 
                 mdl = arima.mod.tran, 
                 fcst = arima.mod.tran.f, 
                 actual = y.test))
}
