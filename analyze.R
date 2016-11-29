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
source('./cross_validate.R')

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

y.pgram <- periodogram(y)
plot(y.pgram$freq,
    pmin(y.pgram$spec, 15000), 
     type = "h", ylab = "Periodogram", 
     xlab = "Period length", lwd = 2, xaxt = 'n', 
    main = 'Periodogram')
axis(1,
     at = c(0,seq(0.1,0.5,by = .1)), 
     labels = sprintf("%4.2f",c(1/y.pgram$freq[1], 1/ seq(0.1,0.5,.1))))

top.spec <- c(3,146)
top.lbs <- sprintf("period: %1.2f", 1/y.pgram$freq[top.spec])
text( y.pgram$freq[top.spec], y = y.pgram$spec[top.spec],
      pos = 4,
     labels = top.lbs )


## plot weather and bike arrivals
plot.ts( data.frame(Temperature = w.train, Arrivals = y.train ),
         main = sprintf("Temperature and Arrivals data for station %s", station.ofchoice) )

# Regression with Arma fitting
reg.arma <- regression.arma(y, w, h = 7)
reg.arma.cv <- cross_validate( y, w, regression.arma, h = 7)
reg.arma.rmses <- sapply(reg.arma.cv, function(x){x$rmse})


plot(w.train, y.train, xlab = 'Temperature', ylab = 'Arrivals')
plot(w.train, BoxCox(y.train, reg.arma$mdl$lambda), 
     xlab = 'Temperature', ylab = 'BoxCox transformed Arrivals', 
     main = sprintf('Arrivals after BoxCox transformation, lambda= %1.2f', reg.arma$mdl$lambda) )

arma.labels <- c('AR parameter',
                 'MA parameter',
                 'Degree of differences')
arma.ix <- c(1,2)
par(mfrow= c(1,2))

for (i in arma.ix){
    hist( sapply(reg.arma.cv, function(x){x$mdl$arma[i]}), 
          main = arma.labels[i],
          xlab = arma.labels[i])    
}
par(mfrow = c(1,1))
reg.arma.mean.rmse <- mean(reg.arma.rmses)
hist( reg.arma.rmses, 
    main = sprintf('RMSE: 7 days ahead\nAverage: %f',reg.arma.mean.rmse),
    xlab = 'RMSE')    

par(mfrow = c(1,1))
reg.arma.fixed.cv <- cross_validate( y, w, regression.arma.fixed, 
                                     h = 7, p = 3, d = 1, q = 3)
reg.arma.fixed.total.rmse <- sqrt(mean(sapply(reg.arma.fixed.cv, function(x){x$rmse^2 * 7})) /7)
hist( sapply(reg.arma.fixed.cv, function(x){x$rmse}), 
      main = sprintf('RMSE: 7 days ahead\nAverage: %f', 
                    reg.arma.fixed.total.rmse) ,
      xlab = 'RMSE')    



Box.test( reg.arma.fixed.cv[[297]]$mdl$residuals, type = 'Ljung-Box', lag = 365, fitdf = 7)
par(mfrow = c(2,2))
plot(reg.arma.fixed.cv[[297]]$mdl$residuals, 
     ylab = 'Residual', 
    main = 'Residuals: model 297')
qqnorm(reg.arma.fixed.cv[[297]]$mdl$residuals, 
     main = 'Histogram of residuals:\nmodel 297',
     xlab = 'Residual error')
qqline(reg.arma.fixed.cv[[297]]$mdl$residuals, col = 'red')
acf(reg.arma.fixed.cv[[297]]$mdl$residuals, lag = 180, 
    main = 'acf plot of residuals: model 297')
pacf(reg.arma.fixed.cv[[297]]$mdl$residuals, lag = 180, 
     main = 'pacf plot of residuals: model 297')


# Tbats 
tbats.mdl <- tbats_single(y, h = 7)
tbats.mdl.cv <- cross_validate(y, w, tbats_single, h = 7)
tbats.rmses <- sapply(tbats.mdl.cv, function(x){x$rmse})
hist(tbats.rmses, main = "Histogram of RMSE", xlab = 'RMSE')

table( sapply(tbats.mdl.cv, function(x){x$mdl[['p']]}))
table( sapply(tbats.mdl.cv, function(x){x$mdl[['q']]}))
par(mfrow = c(1,1))
hist(sapply(tbats.mdl.cv, function(x){
    if (is.null(x$mdl$damping.parameter))
        1
    else
        x$mdl$damping.parameter} ),
    xlab = 'Damping parameter',
    main = 'Histogram of damping parameter'
)

par(mfrow = c(1,2))
hist(sapply(tbats.mdl.cv, function(x){x$mdl$k.vector[1]}),
     main = '# of Fourier terms\nfor weekly seasonality',
     xlab = '# of Fourier terms'
)

hist(sapply(tbats.mdl.cv, function(x){x$mdl$k.vector[2]}),
     main = '# of Fourier terms\nfor annual seasonality',
     xlab = '# of Fourier terms'
)
par(mfrow = c(1,1))


tbats.mdl <- tbats.mdl.cv[[297]]
Box.test( residuals(tbats.mdl.cv[[297]]$mdl), type = 'Ljung-Box', lag = 365, fitdf = 7)
par(mfrow = c(3,2))
plot( residuals(tbats.mdl.cv[[297]]$mdl), 
     ylab = 'Residual', 
     main = 'Residuals: model 297')
qqnorm( residuals(tbats.mdl.cv[[297]]$mdl), 
       main = 'Histogram of residuals:\nmodel 297',
       xlab = 'Residual error')
qqline(residuals(tbats.mdl.cv[[297]]$mdl), col = 'red')
acf(residuals(tbats.mdl.cv[[297]]$mdl), lag = 180, 
    main = 'acf plot of residuals: model 297')
pacf(residuals(tbats.mdl.cv[[297]]$mdl), lag = 180, 
     main = 'pacf plot of residuals: model 297')

acf(residuals(tbats.mdl.cv[[297]]$mdl)^2, lag = 180, 
    main = 'acf plot of squared residuals: model 297')
pacf(residuals(tbats.mdl.cv[[297]]$mdl)^2, lag = 180, 
     main = 'pacf plot of squared residuals: model 297')


tbats.total.rmse <- sqrt(mean(sapply(tbats.mdl.cv, function(x){x$rmse^2 * 7})) /7)
tbats.total.rmse
##############
# Long forecast
############3

tbats.mdl.long.fcst <- tbats_single(y,h = 180)
tbats.mdl.long.fcst$rmse

reg.arma.long.fcst <- regression.arma(y,w,h = 180)
reg.arma.long.fcst$rmse


par(mfrow = c(1,2))
plot( tbats.mdl.long.fcst$fcst,
      main = 'Forecast TBATS\nfor horizon 180 days',
      col = 'blue',
      fcol = 'green') 
legend(x='topright', 
       legend = sprintf("RMSE:%1.2f", tbats.mdl.long.fcst$rmse))
plot(reg.arma.long.fcst$fcst,
     main = 'Forecast\nRegression with ARMA\nfor horizon 180 days',
     col = 'blue',
     fcol = 'green') 
legend(x='topright', 
       legend = sprintf("RMSE:%1.2f", reg.arma.long.fcst$rmse))

# Tbats with arch
tbats.r <- residuals(tbats.mdl.cv[[297]]$mdl)
McLeod.Li.test(y = tbats.r, gof.lag = 20, main = 'McLeod-Li p-values')

?legend
#########################
plot(y, main = "Regression with ARMA:\n7-steps ahead forecast", 
     col = 'blue')

lapply(reg.arma.fixed.cv, function(x){lines(x$fcst$mean, col = 'green')});
legend("topright", legend=c("Actual","Forecast"),
       col = c('blue','green'),
       lty = 'solid')
plot(y, main = "TBATS:\n7-steps ahead forecast", col = 'blue')
lapply(tbats.mdl.cv, function(x){lines(x$fcst$mean, col = 'green')})
legend("topright", legend=c("Actual","Forecast"),
       col = c('blue','green'),
       lty = 'solid')