

data("Hitters",package = "ISLR")
attach(Hitters)
data = na.omit(Hitters)
View(data)
??Hitters
rownames(data)=NULL
View(data)

library(leaps)
View(data)
set.seed(1234)
train_data_index = sample(1:nrow(data),size=round(nrow(data)*0.8),replace = F)

train_data = data[train_data_index,]
test_data = data[-train_data_index,]


fit_train=regsubsets(Salary~.,data=train_data,nvmax=19)

?regsubsets

bic_train = summary(fit_train)$bic

adjr_train = summary(fit_train)$adjr2

cp_train = summary(fit_train)$cp

plot(bic_train,type = 'l')
best_model_index_bic = which(bic_train == min(bic_train));best_model_index_bic

plot(adjr_train,type = 'l')
best_model_index_adjr = which(adjr_train == max(adjr_train));best_model_index_adjr

plot(cp_train,type = 'l')
best_model_index_cp = which(cp_train == min(cp_train));best_model_index_cp

paste("The best model coefficients under BIC :")
best_model_coef_bic = coef(fit_train,id= best_model_index_bic);best_model_coef_bic

paste("The best model coefficients under Adjusted R^2 :")
best_model_coef_adjr = coef(fit_train,id= best_model_index_adjr);best_model_coef_adjr

paste("The best model coefficients under Cp :")
best_model_coef_cp = coef(fit_train,id= best_model_index_cp);best_model_coef_cp



summary(fit_train)

#BIC

lm_bic = lm(Salary ~ AtBat+Hits+Walks+CRBI+Division+PutOuts,data=train_data)
summary(lm_bic)

train_pred_bic = predict(lm_bic,newdata = train_data);train_pred_bic
test_pred_bic = predict(lm_bic,newdata = test_data);test_pred_bic

mse_train_bic = mean((train_data[,19] - train_pred_bic)^2)
mse_train_bic

mse_test_bic = mean((test_data[,19] - test_pred_bic)^2)
mse_test_bic

#Cp

lm_cp = lm(Salary ~ AtBat+Hits+Walks+CHmRun+CRuns+CWalks+Division+PutOuts+Assists+NewLeague,data=train_data)
summary(lm_cp)

train_pred_cp = predict(lm_cp,newdata = train_data);train_pred_cp
test_pred_cp = predict(lm_cp,newdata = test_data);test_pred_cp

mse_train_cp = mean((train_data[,19] - train_pred_cp)^2)
mse_train_cp

mse_test_cp = mean((test_data[,19] - test_pred_cp)^2)
mse_test_cp

#Adj R^2

lm_adjr = lm(Salary ~ AtBat+Hits+Walks+Years+CHmRun+CRuns+CWalks+Division+PutOuts+Assists+NewLeague,data=train_data)
summary(lm_adjr)

train_pred_adjr = predict(lm_adjr,newdata = train_data);train_pred_adjr
test_pred_adjr = predict(lm_adjr,newdata = test_data);test_pred_adjr

mse_train_adjr = mean((train_data[,19] - train_pred_adjr)^2)
mse_train_adjr

mse_test_adjr = mean((test_data[,19] - test_pred_adjr)^2)
mse_test_adjr