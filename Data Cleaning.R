library(ISLR)
Wage
df=Wage
View(df)
attach(df)

nrow(df)

plot(age,wage)
plot(education, wage)
plot(jobclass, wage)
plot(health, wage)

sum(is.na(df)) #no missing values
df_clean=na.omit(df)
nrow(df_clean)

d= Wage[,c(11,2,5,7,8)]
View(d)

model=lm(wage~age+education+jobclass+health,data=d)
e=resid(model)

library(olsrr)

## Influential points
di=cooks.distance(model)
which(di>1)

## Outlier
plot(scale(e))
abline(h=c(-2.5,2.5))
outliers=which(abs(scale(e))>2.5)
length(outliers)

d=d[-outliers,]

nrow(d)
View(d)

## Leverage points
p=4
n=nrow(d)
hii=ols_leverage(model)
l0=3*(p+1)/n
lev=which(hii>l0)

d=d[-lev,]

## Multicollinearity
library(car)
vif(model)
plot(d[,-1])


## Spliting the dataset into three parts
n=nrow(d)

ind=sample(1:n,n)

train_ind=ind[1:round(0.5*n)]
val_ind=ind[(round(0.5*n)+1):round(0.8*n)]
test_ind=ind[(round(0.8*n)+1):n]

train=d[train_in,]
validate=d[val_ind,]
test=d[test_ind,]

lmfit = lm(wage ~ age + as.factor(education) + as.factor(jobclass) + as.factor(health), data = train)
summary(lmfit)

library(stargazer)

stargazer(lmfit, type = "html", title = "Regressing wage on age, education, jobclass, health",
          notes = c("Data has been taken from R"), out = "I://Sem 2//Paper 8//Practical//Exam//wage.html")


## Train MSE
yhat = predict(lmfit, newdata = train)
train.mse = mean((train$wage - yhat)^2); train.mse

# residual plot
e = resid(lmfit); e
plot(yhat, e, type = "p", main = "Residual Plot")
abline(h = 0)

AIC(lmfit)
BIC(lmfit)


## Validation MSE
yhat.val = predict(lmfit, newdata = validate)
validate.mse = mean((validate$wage - yhat.val)^2); validate.mse

## Test MSE

yhat.test = predict(lmfit, newdata = test)
test.mse = mean((test$wage - yhat.test)^2); test.mse
