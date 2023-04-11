library(forecast)
library(smooth)

#create df
expense<- data.frame(c(1,4,6,10,14), c(19,44,40,52,53))
names(expense)<- c("expenditures", "sales")

#scatter graph
plot(expense$expenditures, expense$sales, main="Sales(1000s) vs advertising expenditures (1000s)", 
     xlab="advertising expenditures (1000s)", ylab="sales(1000s)", pch=16, 
     col="orange")

#METHOD: SIMPLE LINEAR REGRESSION 
#model
sales_reg<- lm(sales ~ expenditures, data=expense)
summary(sales_reg)
#plot
plot(expense$expenditures, expense$sales, main="Sales(1000s) vs advertising expenditures (1000s)", 
     xlab="advertising expenditures (1000s)", ylab="sales(1000s)", pch=16, 
     col="orange")
abline(sales_reg, col="blue")

#forecast sales
new_expense<- as.data.frame(8)
colnames(new_expense)<- "expenditures"
predict(sales_reg, newdata = new_expense)
