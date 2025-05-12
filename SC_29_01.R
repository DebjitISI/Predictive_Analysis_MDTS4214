library(MASS)
View(Boston)
attach(Boston)
data = Boston[ , -4]
View(data)
train = data[1:406, ]
test = data[407:506, ]

lfit = lm(medv ~ ., train)
lfit

library(stargazer)
stargazer(lfit, type = "html", 
          title = "Regression of House Price on different predictors", 
          dep.var.labels = "House Price", 
          notes = c("Data as been taken from R"), 
          out = "regression.html")

Y.hat = predict(lfit, train)
Y.hat
train.MSE = mean((train$medv - Y.hat)^2)
train.MSE

#Confidence interval
predict(lfit, test, interval = "confidence")

#prediction interval
predict(lfit, test, interval = "prediction")


AIC(lfit)
BIC(lfit)

library(olsrr)
ols_mallows_cp(lfit, lfit)

##Mannual (to be used)
sigma.sq = 4.868^2

mcp = train.MSE + (2*12*sigma.sq)/406
mcp

##Test MSE
y.hat = predict(lfit, newdata = test)
y.hat
test.MSE = mean((y.hat - test$medv)^2)
test.MSE
