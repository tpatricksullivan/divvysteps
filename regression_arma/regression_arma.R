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

# plot.ts( data.frame(w.train, y.train ))
# BC.lambda <- BoxCox.lambda(y.train)
# plot(w.train, BoxCox(y.train, BC.lambda) )
# 
# lm.mod <- lm( y.train ~ w.train)
# summary(lm.mod)
# plot(w.train, y.train)
# abline(lm.mod)
# plot(lm.mod$residuals, type = 'l')
# plot(lm.mod$fitted, lm.mod$residuals)
# acf(lm.mod$residuals, lag = 180)
# pacf(lm.mod$residuals, lag = 180)
# dwtest(lm.mod)
# Box.test(lm.mod$residuals, type = 'Ljung-Box', lag = 365, fitdf = 2)
# 
# lm.mod.tran <- lm( BoxCox(y.train, BC.lambda) ~ w.train)
# summary(lm.mod.tran)
# plot(w.train, BoxCox(y.train, BC.lambda))
# plot(lm.mod.tran$fitted, lm.mod.tran$residuals)
# acf(lm.mod.tran$residuals, lag = 365)
# pacf(lm.mod.tran$residuals, lag = 365 )
# dwtest(lm.mod.tran)
# Box.test(lm.mod.tran$residuals, type = 'Ljung-Box', lag = 365, fitdf = 3)
# 
# arima.mod <- auto.arima(y.train, xreg = w.train)
# plot(arima.mod$residuals)
# Box.test(arima.mod$residuals, type = 'Ljung-Box', lag = 365, fitdf = 4)
# 
# 
# arima.mod.tran <- auto.arima(y.train, xreg = w.train, lambda = BC.lambda)
# summary(arima.mod.tran)
# plot(arima.mod.tran$residuals)
# qqnorm(arima.mod.tran$residuals)
# qqline(arima.mod.tran$residuals)
# acf(arima.mod.tran$residuals, lag = 365)
# pacf(arima.mod.tran$residuals, lag = 365)
# Box.test(arima.mod.tran$residuals, type = 'Ljung-Box', lag = 365, fitdf = 7)
# arima.mod.tran.f <- forecast(arima.mod.tran, h = 7, xreg = w.test)
# preds <- arima.mod.tran.f$mean
# rmse <- sqrt( sum( (preds - as.numeric(y.test))^2 ) / ( length(y.test)) )
# rmse

