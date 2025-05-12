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

data1 = data.frame(data)
model1 = lm(Sales ~ ., data = data1)
summary(model1)

library(stargazer)
stargazer(model, model1, type = "html", 
          title = "Regression of Sales of Carseats on different quantitative predictors", 
          dep.var.labels = "Sales", 
          notes = c("Data as been taken from R"), 
          out = "regression_carseats.html")

lm1 = lm(Sales ~ Income)
lm2 = lm(Sales ~ CompPrice)
lm3 = lm(Sales ~ Advertising)
lm4 = lm(Sales ~ Population)
lm5 = lm(Sales ~ Price)
lm6 = lm(Sales ~ Age)
lm7 = lm(Sales ~ ., data = data1)
stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, type = "html", 
          title = "Regression of Sales of Carseats on different quantitative predictors", 
          dep.var.labels = "Sales", 
          notes = c("Data as been taken from R"), 
          out = "regression_carseats.html")

#g
mm = sapply(data1, mean)
mm
m = rbind(data, mm) ##adding a row with means of response and the 
#6 predictors

#find out the confidence interval for the last row
predict(lm7, m[401,], interval = "confidence")

#h
#prediction interval for first row = for store 1
predict(lm7, m[1,], interval = "prediction")



##5.
library(ISLR)
View(Credit)
attach(Credit)

#response = Balance
#Nominal = 
lm1 = lm(Balance ~ as.factor(Gender)); lm1
summary(lm1) #significant
lm2 = lm(Balance ~ as.factor(Gender)+ as.factor(Ethnicity)); lm2
lm3 = lm(Balance ~ as.factor(Gender)+ as.factor(Ethnicity) + Income); lm3

stargazer(lm1, lm2, lm3, type = "html", 
          title = "Regression of Balance on different quantitative predictors", 
          dep.var.labels = "Balance", 
          notes = c("Data as been taken from R"), 
          out = "regression_credit.html")

aic = c(AIC(lm1), AIC(lm2), AIC(lm3))
bic = c(BIC(lm1), BIC(lm2), BIC(lm3))
aic; bic
