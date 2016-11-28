library(forecast)
library(ggplot2)
library(sqldf)
library(lmtest)
library(TSA)
source('./load.R', echo=TRUE)
source('./load_weather.R', echo=TRUE)
source('./tbats/tbats.R', echo = TRUE)
source('./regression_arma/regression_arma.R', echo = TRUE)


cross_validate <- function(y, w = NULL, FUN, test = 0.3, h = 7, ... ) {
    train.start <- floor( (1-test) * length(y) )
    train.end <- length(y) - h
    r <- lapply( train.start:train.end, function(x) {
        print(x)
        y.train <- window(y, end = x + h)
        if (!is.null(w)) {
            w.train <- window(w, end = x+h ) 
        } else {
            w.train <- NULL
        }
        res <- FUN(y.train, w.train, h = 7, ...)
        return(res)
    })
    
    return(r)
}
