library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

load_all <- function(data_dir = "data"){
# data_dir = directory (relative to working directory), 
#        assume it has a sub-directory "Output"
# Value
#       returns a data.frame with 
#         time = time
#         station = station
#         count = number of arrivals or departures
#         type = one of c("arrivals", "departues")
        data_dir <- file.path(data_dir, "Output")
        data_files <- file.path(data_dir, list.files(data_dir))
        f <- function(file){
                res <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
                t <- regmatches(file, regexpr("arrivals|departures", file))
                res$type <- as.factor(rep(t, nrow(res)))
                res$station <- rep(gsub("([a-zA-Z/]*)([0-9]+)(.*)","\\2", file), nrow(res))
                colnames(res) <- c("time", "count", "type", "station")
                res$time <- as.Date(res$time, "%Y-%m-%d")
                return(res)
        }
        df  <-  do.call(rbind, lapply(data_files, f))
        return(df)
}
