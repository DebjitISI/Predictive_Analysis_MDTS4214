library(ISLR)
library(class)
library(caret)

data("Weekly")

# Prepare training and testing data
train=Weekly[Weekly$Year<=2008, ]
test=Weekly[Weekly$Year>2008, ]

# Extract predictors and response
x_train=scale(train[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")])
x_test=scale(test[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")])
y_train=train$Direction
y_test=test$Direction

# KNN for K = 2, 5, 9
set.seed(123)
knn_2=knn(x_train, x_test, y_train, k = 2)
knn_5=knn(x_train, x_test, y_train, k = 5)
knn_9=knn(x_train, x_test, y_train, k = 9)

# Confusion matrices and error rates
conf_2=table(Predicted = knn_2, Actual = y_test)
conf_5=table(Predicted = knn_5, Actual = y_test)
conf_9=table(Predicted = knn_9, Actual = y_test)

error_2=mean(knn_2 !=y_test)
error_5=mean(knn_5 !=y_test)
error_9=mean(knn_9 !=y_test)

conf_2; error_2
conf_5; error_5
conf_9; error_9

## Knn Regression
library(kknn)
model=kknn(Volume~Lag1+Lag2+Lag3,train=train,test=test,k=2,kernel="gaussian")
y_hat=fitted(model)
