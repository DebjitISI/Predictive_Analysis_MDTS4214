library(ISLR)
weekly
d = weekly
attach(d)
train = d[Year >= 1990 & Year <= 2008,];train
test = d[Year >= 2009 & Year <= 2010,];test

library(class)
#Response variables (labels)
train_y = train$Direction
test_y = test$Direction

##Scale the predictors
train_scaled = scale(train[,c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")])
test_scaled = scale(test[,c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], 
                    center = attr(train_scaled, "scaled:center"),
                    scale = attr(train_scaled, "scaled:scale"))
##Fit knn
fit = knn(train = train_scaled, test = test_scaled, cl = train_y, k = 2); fit
summary(fit)
fit1 = knn(train = train_scaled, test = test_scaled, cl = train_y, k = 5); fit1
fit2 = knn(train = train_scaled, test = test_scaled, cl = train_y, k = 9); fit2

library(caret)

confusionMatrix(fit, test_y)$table

t = table(Predicted = fit, Actual = test_y) ##Same thing
t
##test error rate
(sum(t) - sum(diag(t)))/sum(t)
