library(forecast)
library(smooth)

#relevant data in a df
enrol<- data.frame(1:6, c(20.5, 20.2,19.5,19,19.1,18.8))
names(enrol)<- c("year", "enrolment")
str(enrol)

#create and plotting a time series
enrol.ts<- ts(enrol[,2], start=1, frequency=1)
plot(enrol.ts, main="Enrolment for the past 6 years in a college", 
     xlab="Years", ylab="Enrolment")

#NAIVE METHOD WITH TREND
naive.trend<- function(x,y){
  x+(x-y)
}
#creating a lagged time series
enrol.lag<- c(NA, enrol.ts[-length(enrol.ts)])
#apply the naive method 
enrol_naive.trend<- c(NA, naive.trend(enrol.ts, enrol.lag))
enrol_naive.trend

#LINEAR TREND EQUATION
enrol_linear.trend<- forecast::tslm(enrol.ts~trend)
enrol_linear.trend$coefficients
enrol_linear.trend$fitted.values
#forecast for the next period:
forecast::forecast(enrol_linear.trend, h=1)

#Plot the observed values and the trend line
plot(enrol.ts, col="blue", lwd=3, xlab="Year", ylab=
       "Enrolment (000s)", main="Enrolment in a college for the past 6 years")
abline(lm(enrol$enrolment ~ enrol$year), col="orange", wd=3, lty=2)
legend("topright", legend = c("Actual", "Linear Trend"), lwd=3, lty=c(1,2), 
       col=c("blue", "orange"))
