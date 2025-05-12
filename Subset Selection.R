library(ISLR)
df=Hitters
attach(df)
df

## 1. Cleaning
dim(df)
df=na.omit(df)
dim(df)

View(df)

## 2. Splitting
set.seed(1234)
train_indices=sample(1:nrow(df),size=0.8*nrow(df))

train=df[train_indices, ]
test=df[-train_indices, ]

## 3. Best Subset Selection
library(leaps)

fit=regsubsets(Salary~.,data=train,nvmax=19,method="exhaustive")
summary(fit)

adjr2=summary(fit)$adjr2; adjr2
aic=summary(fit)$aic; aic
bic=summary(fit)$bic; bic
cp=summary(fit)$cp; cp

# selecting the best model based on the three criterion
best_model_index_adjr2=which(adjr2==max(adjr2)); best_model_index_adjr2
best_model_index_bic=which(bic==min(bic)); best_model_index_bic
best_model_index_cp=which(cp==min(cp)); best_model_index_cp

## 4. Best Models

# getting the best models based on the three criterion
best_model_adjr2=coef(fit,id=best_model_index_adjr2);best_model_adjr2
best_model_bic=coef(fit,id=best_model_index_bic);best_model_bic
best_model_cp=coef(fit,id=best_model_index_cp);best_model_cp

## 5. Train MSE and Test MSE

# Using AdjR2
lm_adjr2=lm(Salary~AtBat+Hits+Walks+Years+CHmRun+CRuns+CWalks+Division+PutOuts+Assists+NewLeague,data=train)
p=predict(lm_adjr2,newdata=train)
q=predict(lm_adjr2,newdata=test)
train_mse=mean((train[,19]-p)^2);train_mse
test_mse=mean((test[,19]-q)^2);test_mse

# Using BIC
lm_bic=lm(Salary~AtBat+Hits+Walks+CRBI+Division+PutOuts,data=train)
p=predict(lm_bic,newdata=train)
q=predict(lm_bic,newdata=test)
train_mse=mean((train[,19]-p)^2);train_mse
test_mse=mean((test[,19]-q)^2);test_mse

# Using Cp
lm_cp=lm(Salary~AtBat+Hits+Walks+CHmRun+CRuns+CWalks+Division+PutOuts+Assists+NewLeague,data=train)
p=predict(lm_cp,newdata=train)
q=predict(lm_cp,newdata=test)
train_mse=mean((train[,19]-p)^2);train_mse
test_mse=mean((test[,19]-q)^2);test_mse

## 6. Compare the values of the best models
summary(best_model_adjr2)
summary(best_model_bic)
summary(best_model_cp)

## To find AIC which is not provided by regsubsets() directly
# $which is a logical matrix showing which predictors are included in the model.
rss=summary(fit)$rss; rss
n=nrow(df)
p=rowSums(summary(fit)$which); p
aic= n*log(rss/n) + 2*p; aic
best_model_index_aic=which(aic==min(aic));best_model_index_aic
best_model_aic=coef(fit,id=best_model_index_aic);best_model_aic
## 7 and 8

#Everything is same for forward and backward subset selection
#just in regsubsets() in place of method = "forward" and method = "backward"

fit_f=regsubsets(Salary ~ ., data = train, nvmax = 19, method = "forward")
summary(fit_f)$bic

fit_b=regsubsets(Salary ~. , data = train, nvmax = 19, method = "backward")
summary(fit_b)$bic
