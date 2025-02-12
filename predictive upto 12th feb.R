library(MASS)
attach(Boston)
View(Boston)
data= Boston[,-4]
View(data)
training_sample= data[1:406, ]# rows should be selected randomly / "sample" function should be used
test_data= data[407:506, ]

linear_fit= lm(medv ~., training_sample);linear_fit
# "." represents all the responses togather excluding medv

library(stargazer)
stargazer(linear_fit, type = "text", title = "Regresion of house price on different predictors", dep.var.labels = "House price", out = "Regression.text")
# if nothing in mentioned 80% should be training data and remaining should be test data
# type should be selected as html for ease of work/ dep.var.labels -> dependent variable name/ out -> output file name
stargazer(linear_fit, type = "html", title = "Regresion of house price on different predictors", dep.var.labels = "House price", out = "Regression.html")

Yhat= predict(linear_fit, training_sample);Yhat
training_MSE= mean((training_sample$medv-Yhat)^2);training_MSE

predict(linear_fit,test_data, interval = "confidence")
predict(linear_fit,test_data, interval = "prediction")

AIC(linear_fit)
BIC(linear_fit)

mallows_cp= training_MSE+ (2*12*var(data$medv-Yhat)/406);mallows_cp
Y_hat= predict(linear_fit,newdata = test_data)
test_MSE= mean((Y_hat-test_data$medv)^2);test_MSE
#------------------------------------------------------------------------------
library(ISLR)
attach(Credit)
plot(Limit,Age) #no linear relationship
plot(Limit,Rating) # clear linear relationship
lmfit1= lm(Balance~Age+Limit+Rating)
summary(lmfit1)
lmfit2= lm(Balance~ Age+Limit)
summary(lmfit2)
