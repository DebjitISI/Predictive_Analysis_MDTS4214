data("Hitters",package="ISLR")
d=Hitters
library(leaps)
attach(d)
View(d)
d
set.seed(1234)
dim(d)
rownames(d)=NULL
d=na.omit(d);dim(d)
x=sample(1:263,263);x
train=d[x[1:floor(0.8*263)],];dim(train)
test=d[x[(floor(0.8*263)+1):263],];dim(test)

##nvmax is the max no. of predictors (say, k)
fit=regsubsets(Salary ~ ., data = train, nvmax = 19, method = "exhaustive")
##it returns k bic, adjr2, cp values corresponding to each subset size (best from each subset,
##with least RSS).
bic=summary(fit)$bic; bic
adjr2=summary(fit)$adjr2; adjr2
cp=summary(fit)$cp; cp
##selecting the best model based on the three criterion
best_model_index_bic = which(bic==min(bic)); best_model_index_bic
best_model_index_adjr2 = which(adjr2==max(adjr2)); best_model_index_adjr2
best_model_index_cp = which(cp==min(cp)); best_model_index_cp

##getting the best models based on the three criterion
best_model_coefs_bic= coef(fit, id = best_model_index_bic);best_model_coefs_bic
best_model_coefs_adjr2= coef(fit, id = best_model_index_adjr2);best_model_coefs_adjr2 
best_model_coefs_cp= coef(fit, id = best_model_index_cp);best_model_coefs_cp 

##fitting the model based on the selected predictors, computing train and test mse
lm_bic=lm(Salary~AtBat+Hits+Walks+CRBI+Division+PutOuts,data=train)
summary(lm_bic)
p=predict(lm_bic,newdata = train)
q=predict(lm_bic,newdata = test)
train_mse_bic=mean((train[,19]-p)^2)
test_mse_bic=mean((test[,19]-q)^2)

lm_adjr2=lm(Salary~AtBat+Hits+Walks+Years+CHmRun+CRuns+CWalks+Division+PutOuts+Assists+NewLeague,data=train)
summary(lm_adjr2)
p=predict(lm_adjr2,newdata = train)
q=predict(lm_adjr2,newdata = test)
train_mse_adjr2=mean((train[,19]-p)^2)
test_mse_adjr2=mean((test[,19]-q)^2)

lm_cp=lm(Salary~AtBat+Hits+Walks+CHmRun+CRuns+CWalks+Division+PutOuts+Assists+NewLeague,data=train)
summary(lm_cp)
p=predict(lm_cp,newdata = train)
q=predict(lm_cp,newdata = test)
train_mse_cp=mean((train[,19]-p)^2)
test_mse_cp=mean((test[,19]-q)^2)

##to get the aic - which is not provided by regsubsets() directly
rss = summary(fit)$rss; rss
n = nrow(d)
summary(fit)$which
p = rowSums(summary(fit)$which); p
aic = n*log(rss / n) + 2*p; aic
##Select the best model - that is with least aic
aic_index = which(aic == min(aic))
##getting hold of the best model - then can use it to fit and compute train and test mse
coef(fit, id = aic_index)


##Everything is same for forward and backward subset selection
##just in regsubsets() in place of method = "forward" and method = "backward"

fit.f=regsubsets(Salary ~ ., data = train, nvmax = 19, method = "forward")
summary(fit.f)$bic

fit.b = regsubsets(Salary ~. , data = train, nvmax = 19, method = "backward")
summary(fit.b)$bic
