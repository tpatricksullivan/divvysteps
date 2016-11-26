# analyze
library(forecast)
library(ggplot2)
library(sqldf)
library(lmtest)
library(TSA)
source('./load.R', echo=TRUE)
source('./load_weather.R', echo=TRUE)
source('./tbats/tbats.R', echo = TRUE)
source('./regression_arma/regression_arma.R', echo = TRUE)

d <- load_all()
w <- load_weather()

#join weather and bike data
orig.rowcount <- nrow(d)
d <- sqldf( "select d.*, w.TMAX 
        from d 
        left outer join w 
        on w.DATE = d.time")
if (orig.rowcount != nrow(d))
        stop("bike data and weather data not merged properly")

# choose station 
station.ofchoice = 333
d.st <- subset(d, station == station.ofchoice & type == "arrivals")

# check number of entries (with adjustment for inclusive counting)
if (nrow(d.st) - 1 != max(d.st$time) - min(d.st$time))
        stop("not all observations included")

# create time series objects
y <- ts(d.st$count, frequency = 1)
w <- ts(d.st$TMAX, frequency = 1)
if (length(y) != length(w))
    stop("bike and weather time series not equal length")


# split into training sets
# choose training set 
h <- 7
y.train <- window(y, end = length(y) - h ) 
y.test <- window(y, start = length(y) - h + 1)
w.train <- window(w, end = length(w) - h ) 
w.test <- window(w, start = length(w) - h + 1)



# plots
## time series plots
p <- ggplot(data = d.st, aes(x = time, y = count))
p <- p + geom_line() + ggtitle( sprintf("Arrivals at Station %s", station.ofchoice))
p

## auto correlation plots
library(TSA)
acf(y.train)
pacf(y.train)
eacf(y.train)

## periodogram plots
y.pgram <- periodogram(y.train)
plot(y.pgram$freq,
    pmin(y.pgram$spec, 15000), 
     type = "h", ylab = "Periodogram", 
     xlab = "Frequency", lwd = 2, xaxt = 'n')
axis(1,at = c(0,seq(0.1,0.5,by = .1)))
top.spec <- c(3,146)
top.lbs <- sprintf("frequency: %1.2f", 1/y.pgram$freq[top.spec])
text( y.pgram$freq[top.spec], y = y.pgram$spec[top.spec]/ 10, 
      pos = 4, 
     labels = top.lbs )


## plot weather and bike arrivals
plot.ts( data.frame(Temperature = w.train, Arrivals = y.train ),
         main = sprintf("Temperature and Arrivals data for station %s", station.ofchoice) )

# Regression with Arma fitting
reg.arma <- regression.arma(y, w, h = 7)

plot(w.train, y.train, xlab = 'Temperature', ylab = 'Arrivals')
plot(w.train, BoxCox(y.train, reg.arma$mdl$lambda), 
     xlab = 'Temperature', ylab = 'BoxCox transformed Arrivals', 
     main = sprintf('Arrivals after BoxCox transformation, lambda= %1.2f', reg.arma$mdl$lambda) )
plot(reg.arma$mdl$residuals)
plot(reg.arma$mdl$x, reg.arma$mdl$residuals)

ts.plot(reg.arma$actual, reg.arma$fcst$mean, gpars=list(col = c('blue', 'red')), 
        ylab = 'Arrivals',
        main = 'Week-ahead forecast')
summary(reg.arma$mdl)

qqnorm(reg.arma$mdl$residuals)
hist(reg.arma$mdl$residuals)
plot(reg.arma$mdl$residuals, type = 'l')
acf(reg.arma$mdl$residuals, lag = 365)
pacf(reg.arma$mdl$residuals, lag = 365)
Box.test(arima.mod.tran$residuals, type = 'Ljung-Box', lag = 365, fitdf = 7)

reg.arma.long.fcst <- regression.arma(y,w,h = 180)
reg.arma.long.fcst$rmse
plot(reg.arma.long.fcst$fcst)

# Tbats 
tbats.mdl <- tbats_single(y, h = 7)
ts.plot(tbats.mdl$actual, tbats.mdl$fcst$mean, gpars = list(col=c('blue','red')))
plot(forecast(tbats.mdl$mdl, h = 180))
plot(residuals(tbats.mdl$mdl))
plot(tbats.components(tbats.mdl$mdl))
Box.test(residuals(tbats.mdl$mdl), type = 'Ljung-Box', lag = 365, fitdf = 6)
acf(residuals(tbats.mdl$mdl), lag = 180)
pacf(residuals(tbats.mdl$mdl), lag = 180)
ts.plot(data.frame(tbats.mdl$mdl$fitted.values, y[8:length(y)]), col = c("red", "blue"))
tbats.mdl$rmse
acf(residuals(tbats.mdl$mdl)^2, lag = 180)
pacf(residuals(tbats.mdl$mdl)^2, lag = 180)
hist(residuals(tbats.mdl$mdl))

tbats.mdl.long.fcst <- tbats_single(y,h = 180)
tbats.mdl.long.fcst$rmse
plot( tbats.mdl.long.fcst$fcst) 

# Tbats with arch
tbats.r <- residuals(tbats.mdl$mdl)
McLeod.Li.test(y = tbats.r, gof.lag = 180)


#########################

