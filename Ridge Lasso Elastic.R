library(ISLR)

df=Hitters
df=na.omit(df)
rownames(df) = NULL
dim(df)
View(df)

#Need of model matrix:
#It will contain all values of the predictors
#using glmnet, if there is any text value, it won't work
#So we use model matrix as it would convert all text values into dummy variables

##prepare the model matrix
x=model.matrix(Salary ~.,data=df)[,-1] ## -1 to remove the intercepts
x
dim(x)
y=data$Salary

## 1. Splitting
set.seed(123)
test_index=sample(1:nrow(df),100)
test=df[test_index,]
train=df[-test_index,]

## 2. Least Square
model_ls=lm(Salary~.,data=df)
p=predict(model_ls,newdata=train)
q=predict(model_ls,newdata=test)
train_mse=mean((train[,19]-p)^2);train_mse
test_mse=mean((test[,19]-q)^2);test_mse

## 3. Ridge Regression
library(glmnet)

# grid of values - grid of size 100
grid =10*seq(10,2,length = 100); grid

model_r=glmnet(train[,-19],train[,19],alpha=0, type.measure="mse",lambda=grid,family="gaussian")
model_r

# dimension of coefficient matrix
coef_mat=coef(model_r)
dim(coef_mat) # dimension of coefficient matrix is 20x100

# lambda corresponding to 50th and 60th choice
grid[50]
grid[60]

# or
model_r$lambda[50]
model_r$lambda[60]

# l2 norm
c1=coef(model_r)[2:20,50];c1
c2=coef(model_r)[2:20,60];c2

c1_l2=sqrt(sum(c1^2));c1_l2
c2_l2=sqrt(sum(c2^2));c2_l2
# Interpret: As lambda increases, coefs shrink towards 0, therefore c1_12<c2_l2

# Conclusion
# Higher lambda, leads to smaller l2 norm imposing stronger regularization, shrinking the
# estimates of the coefficients towards zero and hence reducing the over fitting.

# MSE
y_hat_test1=predict(model_r,newx=x[test_index,],s=60.40404)
test_mse1=mean((y_hat_test1-y[test_index])^2);test_mse1

y_hat_test2=predict(model_r,newx=x[test_index,],s=52.32323)
test_mse2=mean((y_hat_test2-y[test_index])^2);test_mse2
# for these two choices of lambda - test mse decrease.

## 4. Plot predictors vs lambda
plot(model_r,xvar="lambda",label=TRUE,main="Plot")

# IF U TRY TO PLOT COEFS ag LAMBDA - shrinkings of the predictors across the different
# values of predictor will not be visible clearly - instead log lambda works

#lam=model_r$lambda #lambda values
#cef=as.matrix(model_r$beta)   #coefficient matrix
#matplot(lam, t(cef), type = "l")

## 5. 
model_r$beta  #it does not give the intercept
coef(model_r)  #it gives regularized coefficients

##When trying to do it in a natural way that is using the above fitted models - a prob was
##occurring that the ratio was going above 1, possible reason might be ridge always works 
##with standardized predictors but OLS does not, as result standardizing the predictors
##beforehand then fitting the models and obtaining the L2 norm for OLS and L2 norm for
##ridge for the different values of lambda and taking their ratio which would be between 0 and 1
##and finally plotting that against the coefficients (std coefs)

# Standardise
x_scaled=scale(x)
y_scaled=scale(y)

x_train=x_scaled[-test_index,]
y_train=y_scaled[-test_index]
x_test=x_scaled[test_index,]
y_test=y_scaled[test_index]


# OLS
ols=lm(y_train~x_train)

# Ridge
ridge_model=glmnet(x_train,y_train,alpha=0,lambda=grid)


## 6. Cross Validation for Ridge
cv_ridge=cv.glmnet(x_train,y_train,lambda=grid,alpha=0,nfold=5)
optimal_lambda_ridge=cv_ridge$lambda.min; optimal_lambda_ridge
ridge_model_op=glmnet(x_train,y_train,alpha=0,lambda=optimal_lambda_ridge)
y_hat_test_ridge=predict(ridge_model_op,s=optimal_lambda_ridge,newx=x_test)
test_mse_ridge=mean((y_test-y_hat_test)^2);test_mse

## 7. Cross Validation for Lasso
cv_lasso=cv.glmnet(x_train,y_train,lambda=grid,alpha=1,nfolds=5)
optimal_lambda_lasso=cv_lasso$lambda.min;optimal_lambda_lasso
lasso_model_op=glmnet(x_train,y_train,alpha=1,lambda=optimal_lambda_lasso)
y_hat_lasso=predict(lasso_model_op,s=optimal_lambda_lasso,newx=x_test)
test_mse_lasso=mean((y_hat_lasso-y_test)^2);test_mse_lasso

# Number of non-zero coefficient estimates
sum(lasso_model_op$beta!=0)

## 8. Cross Validation for Elastic Net
cv_elastic=cv.glmnet(x_train,y_train,lambda=grid,alpha=0.5,nfolds=5)
optimal_lambda_elastic=cv_elastic$lambda.min; optimal_lambda_elastic
elastic_model_op=glmnet(x_train,y_train,lambda=optimal_lambda_elastic,alpha=0.5)
y_hat_elastic=predict(elastic_model_op,s=optimal_lambda_elastic,newx=x_test)
test_mse_elastic=mean((y_hat_elastic-y_test)^2);test_mse_elastic

# Number of non-zero coefficient estimates
sum(elastic_model_op$beta!=0)

## 9.
# model with least test MSE will be considered the best model - predict the salary best
# The best method is the one with the lowest Test MSE.
# If Lasso gives the lowest MSE, then some variables are likely irrelevant.
# If Ridge performs best, then most predictors contribute to the response.
# If Elastic Net performs best, then a combination of both strategies works well.