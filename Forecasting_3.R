library(forecast)
library(smooth)

#creating relevant data
sales<- c(1690, 940, 2625, 2500, 1800, 900, 2900, 2360, 1850, 1100, 2930, 2615)
sales

#create and plot the time series
#1 using autoplot
sales.ts<- ts(sales, start=c(1,1), frequency=4)
autoplot(sales.ts)
#2 using seasonplot
ggseasonplot(sales.ts)
#3 using plot
plot(sales.ts, type="b", pch=19)
abline(v=c(1,2,3), col="blue", lty=2)

#METHOD: FOUR-QUARTER MOVING AVERAGE
sales.ma4<- sma(sales.ts, order=4, b=1)
#extracting the fitted values and creating a time series object
sales_ma.fitted<- ts(sales.ma4$fitted[5:12], start=c(2,1), frequency=4)
sales_ma.fitted
#determine the forecast
forecast(sales.ma4)

#plotting both observed and fitted values 
plot(sales.ts, type="b", pch=19)
lines(sales_ma.fitted, col="blue")
legend("topleft", legend = c("Actual", "MA(4)"), col=c("black", "blue"), lty=1,
       cex=0.6)

#METHOD: CENTERED MOVING AVERAGE (4)
sales.cma4<- ma(sales.ts, order=4,centre = TRUE)
sales.cma4
#decompose and plot the time series
decomposed_sales<- decompose(sales.ts, type="multiplicative")
autoplot(decomposed_sales)

#Seasonal indexes
decomposed_sales$figure
#plot
plot(decomposed_sales$figure, col="darkgreen")
abline(h=1, col="blue", lty=2)
