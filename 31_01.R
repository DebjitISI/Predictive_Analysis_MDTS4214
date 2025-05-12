##1.

set.seed(123)

X = 5:10
Y = 2 + 3*X
plot(X,Y, type = "l", xlab = "predictor", ylab = "response", main = "population vs sample regression line")

for (i in 1:5) {
  n = 50
  x = runif(n, min = 5, max = 10); x
  res = rnorm(n, mean = 0, sd = 4); res
  y = 2 + 3*x + res; y
  
  model = lm(y~x)
  abline(model, col = i)
}

legend("topleft", legend = c("Population", "Sample1", "Sample2", "Sample3", "Sample4", "Sample5"), col = 1:5, lty = 1)

##2.
set.seed(123)
n = 50
x2 = runif(n, min = 5, max = 10)
x1 = x2 - mean(x2)
res1 = rnorm(n, mean = 0, sd = 1)
y1 = 2 + 3*x1 + res1

model = lm(y1~x1)
cf = coef(model); cf

beta0 = c(seq(0,4,0.1),cf[1])
beta1 = c(seq(1,5,0.1),cf[2])
length(beta0); length(beta1)
rss = matrix(0,nrow = 42, ncol = 42)

for (i in 1:42) {
  for (j in 1:42){
    rss[i,j] = sum((y1 - beta0[i] - beta1[j]*x1)^2)
  }
  
}
rss
min(rss)
which(rss == min(rss), arr.ind = TRUE)


##3.

set.seed(123)
n = 50

m = matrix(0, nrow = R, ncol = 2)
R = 100
for (i in 1:R) {
  x3 = runif(n, min = 0, max = 1)
  res3 = rnorm(n, mean = 0, sd = 1)
  y3 = 2 + 3*x3 + res3
  
  model = lm(y1~x1)
  m[i,] = coef(model)
}

m
b0.hat = mean(m[,1]); b0.hat
b1.hat = mean(m[,2]); b1.hat

##4.

library(ISLR)
attach(Carseats)
View(Carseats)

#not quantitative - Edc, Urban, Us, Shelveloc

data = Carseats[,-c(7,9,10,11)]; data
model = matrix(0, nrow = 6, ncol = 2)
for (i in 1:6) {
  fit = lm(Sales ~ data[,i+1])
  model[i,] = coef(fit)
}
model