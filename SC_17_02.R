set.seed(123)
n = 1000
x1 = rnorm(n, 0 ,2)
x2 = rpois(n, 1.5)
epsilon = rnorm(n)
y = -2 + 1.4*x1 - 2.6*x2 + epsilon
data = data.frame(y, x1, x2)
train = data[1:800,]
test = data[-c(1:800),]
dim(test)

lmfit = lm(y ~ x1 + x2, data = train)
#to get yhat = bohat + blhat*x1 + b1hat*x2
yhat = predict(lmfit, newdata = test) 
test_mse_lmfit = mean((yhat - test$y)^2)
test_mse_lmfit

library(kknn)

kfit = kknn(y ~ x1+x2, train, test, k=15)
yhat.kfit = predict(kfit, newdata = test)
test_mse_kfit = mean((yhat.kfit - test$y)^2)
test_mse_kfit

#setup2
y_new = -2 + 1.4*x1 - 2.6*x2 + 2.9*x1^2 + 3.1*exp(x2) - 1.5*x1*x2^2 + epsilon
data_new = data.frame(y_new, x1, x2)

train_new = data_new[1:800,]
test_new = data_new[-c(1:800),]

lmfit_new = lm(y_new ~ x1 + x2, data = train_new)
yhat_new = predict(lmfit_new, newdata = test_new)

test_mse_new = mean((yhat_new - test_new$y_new)^2)
test_mse_new

kfit_new = kknn(y_new ~ x1+x2, train_new, test_new, k=15)
yhat.kfit_new = predict(kfit_new, newdata = test_new)
test_mse_kfit_new = mean((yhat.kfit_new - test_new$y_new)^2)
test_mse_kfit_new