library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

load_weather <- function(file = './data/841451.csv', 
                         station = 'CHICAGO OHARE INTERNATIONAL AIRPORT IL US'){
        # data_dir = directory (relative to working directory), 
        res <- read.csv(file, stringsAsFactors = FALSE)
        res <- res %>%
                mutate( DATE = as.Date(as.character(DATE),'%Y%m%d')) %>%
                mutate( STATION_NAME = as.factor(STATION_NAME)) %>%
                select(DATE, TMAX, STATION_NAME) %>%
                filter( TMAX != -9999 & STATION_NAME == station)
        return(res)
}

# p <- ggplot(data = res, mapping = aes(x=DATE, y = TMAX)) 
# p <- p + geom_line()
# p <- p + facet_wrap(~STATION_NAME)
# p