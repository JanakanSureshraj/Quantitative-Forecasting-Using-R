setwd("C:\\Users\\ACER\\Downloads")

#package stats for time series forecasts
install.packages("stats")
library(stats)
#package forecast for displaying and analysing univariate time series forecasts
install.packages("forecast")
library(forecast)
#package smooth for smoothing functions in forecasting
install.packages("smooth")
library(smooth)

rate<- read.csv("data.csv")
str(rate)

#ts function to create a time series object
#start= time of the first observation. frequency= number of observations per unit of time
rate.ts<- ts(rate[,2], start = 1, frequency = 12)

#plot the time series 
ggseasonplot(rate.ts, main="interest rates over 12 months", xlab="months", ylab="interest rate")

#SIMPLE FORECASTING METHODS
#Forecast Method: Naive Method
rate_naive<- naive(rate.ts, h=1)
summary(rate_naive)
#Forecast Method: Simple Mean Method
rate_mean<- meanf(rate.ts, h=1)
summary(rate_mean)

#SMOOTHING FORECASTING METHODS
#Smoothing Method: Moving Average (3) (three-month)
rate_ma3<- sma(rate.ts, order=3, h=1)
forecast(rate_ma3, 1)

#FORECAST ACCURACY MEASURES 
#Measure: Mean Squared Error (MSE)
mse_value<- function(error){
  mean(error^2)
}
#Measure: Mean Average Deviation (MAD)
mad_value<- function(error){
  mean(abs(error))
}
#Measure: Mean Average Percentage Error (MAPE)
mape_value<- function(error, obs){
  mean(abs(error/obs))*100
}

#CALCULATING FORECASTS ERRORS AND VALUES FOR THE MA(3) CALCULATED
#determine forecast errors
forecast.err.ma3<- rate_ma3$residuals[4:12]

#MSE for MA(3)
mse_ma3<- mse_value(forecast.err.ma3)
mse_ma3
#MAD for MA(3)
mad_ma3<- mad_value(forecast.err.ma3)
mad_ma3
#MAPE for MA(3)
mape_ma3 <- mape_value(forecast.err.ma3,rate_ma3$y[4:12])
mape_ma3

#Smoothing Method: Moving Average (4) (four-month)
rate_ma4<- sma(rate.ts, order=4, h=1)
forecast(rate_ma4, 1)

#CALCULATING FORECASTS ERRORS AND VALUES FOR THE MA(4) CALCULATED
#determine forecast errors
forecast.err.ma4<- rate_ma4$residuals[5:12]

#MSE for MA(4)
mse_ma4<- mse_value(forecast.err.ma4)
mse_ma4
#MAD for MA(4)
mad_ma4<- mad_value(forecast.err.ma4)
mad_ma4
#MAPE for MA(4)
mape_ma4 <- mape_value(forecast.err.ma4,rate_ma3$y[5:12])
mape_ma4


#df for plotting 

rate_ma3.mod<- rate_ma3$fitted
rate_ma3.mod[1:3]<- c(NA, NA, NA)
rate_ma3.mod

rate_ma4.mod<- rate_ma4$fitted
rate_ma4.mod[1:4]<- c(NA, NA, NA, NA)
rate_ma4.mod

my_data<- cbind.data.frame(rate, rate_naive$fitted, rate_ma3.mod, rate_ma4.mod)
names(my_data)<- c("month", "interest_rates", "naive_fitted", "MA3_fitted", "MA4_fitted")

#plotting the time series
#1st time series
plot(my_data$month, my_data$interest_rates, 
     col=2, xlab="Months", ylab="interest rates")

#2nd time series
lines(my_data$month, my_data$naive_fitted, 
     col=3)

#3rd time series
lines(my_data$month, my_data$MA3_fitted, 
      col=4)

#4th time series
lines(my_data$month, my_data$MA4_fitted,
      col=5)

#adding legend
legend("topleft",c("observed", "naive", "MA3", "MA4"),
       lty = 1,
       col = 2:5)
