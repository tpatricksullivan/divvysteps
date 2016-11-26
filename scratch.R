# scratch



# periodogram of station arrivals
library(TSA)
y <- ts(d.st$count, frequency = 1)
y.365 <- window(y,start = 1, end = 365*2)
spec.pgram(y)
y.periodogram <- TSA::periodogram(y)
y.periodogram
top.10 <- order(y.periodogram$spec, decreasing = T)[1:10]
top.10.freq <- 1 / y.periodogram$freq[top.10]
spectrum(y)
spec.max <- max(y.periodogram$spec)
spec.max.at.freq <- y.periodogram$freq[which.max(y.periodogram$spec)]
c(spec.max, spec.max.at.freq)
head(y.periodogram)
?periodogram

# plot 
library(plotly)
p <- plot_ly(data.frame(x=1:1000, y = y[1:1000]), x = ~x, y = ~y, type = 'scatter', mode= 'lines')
p <- plot_ly(data.frame(x=1 / y.periodogram$freq, y = y.periodogram$spec), x = ~x, y = ~y, type = 'scatter', mode= 'lines')

Sys.setenv("plotly_username"="tpatricksullivan")
Sys.setenv("plotly_api_key"="ogxnhu0yyx")
plotly_POST(p, filename = "test_periodogram")





# auto.arima with weekly seasonality
acf(y.train)
acf(diff(y.train, lag = 7, differences = 3))
pacf(y.train)
y.train.wkly <- ts(y.train, frequency = 7)
y.model.wkly <- auto.arima(y.train.wkly)
summary(y.model.wkly)
y.model.wkly.preds <- as.numeric(forecast(y.model.wkly, h = length(y.test))$mean)
y.model.wkly.rmse <- sqrt( sum( (y.model.wkly.preds - as.numeric(y.test))^2 ) / ( length(y.test)) )
plot(as.numeric(y.test), col = 'red', type = 'l')
lines(y.model.wkly.preds, type = 'l')
y.model.wkly.rmse

# auto.arima with weekly and yearly seasonality
yearly.lag <- 365
y.train.yrlydiff <- diff(y.train, lag = yearly.lag)
plot(y.train.yrlydiff)
y.model.yrly <- auto.arima(y.train.yrlydiff)
summary(y.model.yrly)
fcst.length <- length(y.test)
y.model.yrlydiff.f <- forecast(y.model.yrly, h = fcst.length)$mean
actual.lags <- y.train[ (length(y.train) - yearly.lag + 1) : (length(y.train) - yearly.lag + fcst.length) ]
y.model.yrly.preds <- as.numeric(y.model.yrlydiff.f + actual.lags )
y.model.yrly.rmse <- sqrt( sum( (y.model.yrly.preds - as.numeric(y.test))^2 ) / ( length(y.test)) )
plot(as.numeric(y.test), col = 'red', type = 'l')
lines(y.model.yrly.preds, type = 'l')
y.model.yrly.rmse


##weekly differencing 
y.train.wkly <- ts(y.train, frequency = 7)
arima.mod <- auto.arima(y.train.wkly, xreg = w.train, lambda = BC.lambda)
plot(arima.mod$residuals)
acf(arima.mod$residuals, lag = 90)
pacf(arima.mod$residuals, lag = 90)
qqline(arima.mod$residuals)
Box.test(arima.mod$residuals)
fcst.length <- length(y.test)
arima.mod.f <- forecast(arima.mod, h = fcst.length, xreg = w.test)
plot(arima.mod.f)
preds <- arima.mod.f$mean 
rmse <- sqrt( sum( (preds - as.numeric(y.test))^2 ) / ( length(y.test)) )
rmse
plot( sqrt((preds - as.numeric(y.test))^2))

# tbats
?msts

y.msts <- msts(y.train, seasonal.periods = c(7,365))

mdl.tbats <- tbats(y.msts)
mdl.tbats.f <- forecast(mdl.tbats,h = 180)
preds <- mdl.tbats.f$mean
rmse <- sqrt( sum( (preds - as.numeric(y.test))^2 ) / ( length(y.test)) )
rmse
plot(mdl.tbats.f)
plot(y.test)
lines(1002:1008, preds)

# hw
x <- ts( d_333_train$count, start = start_time_d, frequency = 1)
plot(x)
x_hw_m <- hw( x, h = 365, seasonal = 'multiplicative') # error due to nonseasonal data
x_hw_m <- hw( ts(x+1) , h = 365, seasonal = 'multiplicative')
plot(forecast(x_hw_m))

x_hw_m <- hw( ts( window(x+1, start = 2441) ,frequency = 7) , h = 52, seasonal = 'multiplicative')
plot(forecast(x_hw_m))

# auto correlation

acf( ts(x, frequency = 1),lag.max = 365)
pacf( ts(x, frequency = 1), lag.max = 365)

# seasonal arima
x <- ts( d_333_train$count, start = start_time_d, frequency = 1)
auto.arima( x)

x.diff <- diff(x,lag = 365)
m_arima <- auto.arima( ts(x.diff,frequency = 7) )
plot(forecast(m_arima,h=365))

x.log <- log(x+1) + 1
plot.ts(x.log)
x.log.diff <- diff(x.log, lag = 365)
plot(x.log.diff)
acf(x.log.diff, lag = 365)
pacf(x.log.diff, lag = 365)
log.arima <- auto.arima( ts(x.log.diff, frequency = 7) )
log.arima.f <-  forecast(log.arima,h = 52)
plot(log.arima.f)
log.arima.p <- exp( diffinv(log.arima.f$mean, lag = 365) - 1) -1
par(mfrow=c(2,1))
plot.ts(log.arima.p)
plot.ts(d_333$count)
window

x <- ts( d_333_train$count, start = start_time_d, frequency = 1)
lambda <- BoxCox.lambda(x+1)
x.BC <- BoxCox(x+1, lambda)
par(mfrow=c(2,1))
plot.ts(x+1)
plot.ts(BoxCox(x+1, lambda))
x.BC.diff <- diff( x.BC, lag = 365)
acf(x.BC.diff)
pacf(x.BC.diff)
mod <- auto.arima( x.BC.diff )
mod.f <- forecast(mod, h = 365)
par(mfrow=c(1,1))
plot(mod.f)

e <- rnorm(1000)
plot.ts(e)
plot(forecast(auto.arima(e), h = 100))

# ets
library(forecast)
?ets

x.ets <- ets(ts(x.diff+1,frequency = 7),  model = "MNM")
plot(forecast(x.ets, h = 365))

#naive
y.sn <- snaive( ts(y.train, frequency = 365), h = 7)
y.sn.BC <- snaive(y, h = 7, lambda =BC.lambda)
par(mfrow=c(2,1))
plot(y.sn)
sqrt(sum(( as.numeric(y.sn$mean) - as.numeric(y.test))^2) / 7)
plot.ts(d_333$count)

# aggregated

?seq_along
.day <- rep(1:365, length.out = length(y))
auto.arima( ts(y,frequency = 7), xreg = .day)

#####################

# Exploratory analysis
library(ggplot2)
source('./load.R', echo=TRUE)

d <- load_all()

p <- ggplot( data = d[d$type == 'arrivals',], mapping = aes(x=time, y= count))
p <- p + geom_line(size = 0.3)
p <- p + facet_grid(station ~ type)
dev.new()
p
