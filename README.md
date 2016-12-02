# divvysteps
We completed this project for a class in time series analysis as part of the [University of Chicago MSc Analytics](https://grahamschool.uchicago.edu/credit/master-science-analytics/index) program. 

# Problem
The Chicago bike-sharing program, Divvy, needs to maintain a balance of bikes at different stations. They need to be able to forecast demand at different stations and make sure that bikes are available. They would like to avoid cases where bikers need to return a bike and the station is already full or where bikers need a bike and none are available at that station. 

# Approach
We forecasted arrivals at a particular Divvy station. We took a time series modeling approach with three different families of models.
* Regression on weather data with ARMA errors
* TBATS
* Recurrent Neural Networks
