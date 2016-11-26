library(forecast)
library(dplyr)

tbats_single <- function(y, h = 7) {
        # y: time series input
        # h: horizon to forecast over
        # splits y into test and training data sets
        # with h observations in the test set and the remaining in the training set
        y.train <- window(y, end = length(y) - h ) 
        y.test <- window(y, start = length(y) - h + 1)
        y.msts <- msts(y.train, seasonal.periods = c(7,365))
        mdl <- tbats(y.msts)
        mdl.f <- forecast(mdl,h = h)
        preds <- mdl.f$mean
        rmse <- sqrt( sum( (preds - as.numeric(y.test))^2 ) / ( length(y.test)) )
        res <- list(rmse = rmse, mdl = mdl)
        return(res)
}

tbats_all <- function(d, h = 7) {
        d <- d[d$type=='arrivals',]
        d.ts <- lapply(split(d,d$station), function(x){ts(x$count)} )
        res <- lapply(d.ts, tbats_single, h = h)
        return(res)
}

