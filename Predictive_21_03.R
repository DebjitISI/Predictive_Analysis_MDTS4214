library(ISLR)
attach(Hitters)
View(Hitters)
dim(Hitters)
data = na.omit(Hitters)
dim(data)
rownames(data) = NULL
View(data)
##to check for NAs -> sum(is.na(Hitters))

#Need of model matrix:
#It will contain all values of the predictors
#using glmnet, if there are any text values, it won't work
#So we use model matrix as it would convert all text values into dummy variables

##prepare the model matrix
x = model.matrix(Salary ~., data)[,-1] ## -1 to remove the intercepts
x
View(x)
dim(x)
y = data$Salary

##train-test split
set.seed(seed=123)

unit = 1:nrow(data)
test_units = sample(unit, 100, replace = FALSE)

train_units = unit[-c(test_units)]
length(test_units); length(train_units)

#ols
##Fitting least square reg of salary on all other variables on the train set.
fit_ols = lm(y[train_units] ~ ., data = data.frame(y = y[train_units], x[train_units, ]))
##y_hat values from test data set
y_hat_test_ols = predict(fit_ols, newdata = data.frame(x[test_units, ]))
##test mse
test_mse = mean((y[test_units]-y_hat_test_ols)^2);test_mse  
##y_hat values from train data set
y_hat_train_ols = predict(fit_ols, newdata = data.frame(x[train_units, ]))
##train mse
train_mse = mean((y[train_units] - y_hat_train_ols)^2); train_mse

##The predict() of lm - newdata argument takes dataframe
##The predict() of glmnet - newx argument takes matrix

##Ridge Regression
install.packages("glmnet")
library(Matrix)
library(glmnet)

##grid of values - grid of size 100
grid = 10^seq(10,-2,length = 100); grid
##Fit the ridge reg
ridge.model = glmnet(x[train_units,], y[train_units], alpha = 0, lambda = grid)
##alpha = 0 for ridge reg
c = coef(ridge.model) ##coefficients of the ridge reg model
dim(c) ##Dim of the coefficient matrix - 20x100

ridge.model$lambda ##All the 100 values of lambda
##Value of lambda corresponding to the 50th & 60th choice of lambda
ridge.model$lambda[50]
ridge.model$lambda[60]

##Computing the l2 norm corresponding to the 50th, 60th choice of lambda (value)
c1 = coef(ridge.model)[-1,50]; c1 ##intercept not needed.
c2 = coef(ridge.model)[-1,60]; c2

l2norm.1choice = sqrt(sum(c1^2)); l2norm.1choice
l2norm.2choice = sqrt(sum(c2^2)); l2norm.2choice
##As lambda increases, coefs shrink more towards 0, therefore l2norm.1stchoice < l2norm.2ndchoice

##Conclusion
##Higher lambda, leads to smaller l2 norm imposing stronger regularization, shrinking the
##estimates of the coefficients towards zero and hence reducing the over fitting.

yhat.test.1 = predict(ridge.model, s = 11497.57, newx = x[test_units,])
test.mse.1 = mean((yhat.test.1 - y[test_units])^2); test.mse.1

yhat.test.2 = predict(ridge.model, s = 705.4802, newx = x[test_units,])
test.mse.2 = mean((yhat.test.2 - y[test_units])^2); test.mse.2
##for this two choice of lambda - test.mse decreases.

##4. plot of standardized coefficients against grid values of lambda
plot(ridge.model, xvar = "lambda", label = TRUE)
title(main = "Standardised coefs ag lambda (log lambda)", font.main = 2)

##IF U TRY TO PLOT COEFS ag LAMBDA - shrinkings of the predictors across the different
##values of predictor will not be visible clearly - instead log lambda works
#lam = ridge.model$lambda #lambda values
#cef = as.matrix(ridge.model$beta)   #coefficient matrix
#matplot(lam, t(cef), type = "l")


##5. plot of std coefs with l2 norm ratio

ridge.model$beta ##does not gives the intercept
coef(ridge.model) ##returns the intercept along with the regularized coefficients

##When trying to do it in a natural way that is using the above fitted models - a prob was
##occuring that the ratio was going above 1, possible reason might be ridge always works 
##with standardized predictors but ols does not, as result standardizing the predictors
##beforehand then fitting the models and obtaining the L2 norm for ols and L2 norm for
##ridge for the different values of lambda and taking there ratio which would be btw 0 and 1
##and finally plotting that against the coefficients (std coefs)

# Standardize x manually before fitting both models
x_train <- scale(x[train_units, ])
y_train <- y[train_units]
# Fit Ridge Regression (alpha = 0) - Disable internal standardization
fit_ridge <- glmnet(x_train, y_train, alpha = 0, lambda = grid, standardize = FALSE)
# Fit OLS for comparison
fit_ols <- lm(y_train ~ x_train, data = data.frame(y = y_train, x_train))  
beta_ols <- coef(fit_ols)[-1]; beta_ols  # Remove intercept manually
l2_norm_ols <- sqrt(sum(beta_ols^2))  # Compute L2 norm of OLS coefficients
# Compute L2 norm of Ridge regression coefficients for each lambda
l2_norm_ridge <- apply(as.matrix(fit_ridge$beta), 2, function(beta) sqrt(sum(beta^2))); l2_norm_ridge
# Compute ratio of L2 norms (should be between 0 and 1)
l2_ratio <- l2_norm_ridge / l2_norm_ols; l2_ratio
# Plot standardized coefficients vs. L2 norm ratio
matplot(l2_ratio, t(as.matrix(fit_ridge$beta)), type = "l", lty = 1, col = rainbow(nrow(fit_ridge$beta)),
        xlab = "L2 Norm Ratio)", 
        ylab = "Standardized Coefficients", 
        main = "Standardized Coefficients vs L2 Norm Ratio")
# Add legend
legend("topright", legend = rownames(as.matrix(fit_ridge$beta)), col = rainbow(nrow(fit_ridge$beta)), lty = 1, cex = 0.7)


##6.cross validation for ridge
#alpha = 0 for ridge
set.seed(seed=123)
cv.ridge = cv.glmnet(x[train_units,], y[train_units], lambda = grid, nfolds = 5, alpha = 0)
opt.lambda.ridge = cv.ridge$lambda.min; opt.lambda.ridge ##optimal lambda
ridge.model.cv = glmnet(x[train_units,], y[train_units], lambda = opt.lambda.ridge, alpha = 0)
ridge.model.cv$beta

y_hat_ridge_cv = predict(ridge.model.cv, s = opt.lambda.ridge, newx = x[test_units,])
test_mse_ridge_cv = mean((y_hat_ridge_cv - y[test_units])^2); test_mse_ridge_cv            

##7. cross validation for lasso
#alpha = 1 for lasso
set.seed(seed=123)
cv.lasso = cv.glmnet(x[train_units,], y[train_units], lambda = grid, nfolds = 5, alpha = 1)
opt.lambda.lasso = cv.lasso$lambda.min; opt.lambda.lasso ##optimal lambda
lasso.model.cv = glmnet(x[train_units,], y[train_units], lambda = opt.lambda.lasso, alpha = 1)

##Number of Non Zero Coefficients (2 ways)
sum(lasso.model.cv$beta != 0)
sum(coef(lasso.model.cv, s = opt.lambda.lasso) != 0) - 1 ##-1 for excluding the intercept

y_hat_lasso_cv = predict(lasso.model.cv, s = opt.lambda.lasso, newx = x[test_units,])
test_mse_lasso_cv = mean((y_hat_lasso_cv - y[test_units])^2); test_mse_lasso_cv

##8. cross validation for elastic net
#alpha = 0.5 for giving equal weight age to ridge and lasso - can be adjusted accordingly
set.seed(seed=123)
cv.elastic = cv.glmnet(x[train_units,], y[train_units], lambda = grid, nfolds = 5, alpha = 0.5)
opt.lambda.elastic = cv.elastic$lambda.min; opt.lambda.elastic ##optimal lambda
elastic.model.cv = glmnet(x[train_units,], y[train_units], lambda = opt.lambda.elastic, alpha = 0.5)

##Number of Non Zero Coefficients (2 ways)
sum(elastic.model.cv$beta != 0)
sum(coef(elastic.model.cv, s = opt.lambda.elastic) != 0) - 1 ##-1 for excluding the intercept

y_hat_elastic_cv = predict(elastic.model.cv, s = opt.lambda.elastic, newx = x[test_units,])
test_mse_elastic_cv = mean((y_hat_elastic_cv - y[test_units])^2); test_mse_elastic_cv

##9.
##model with least test mse will be considered the best model - predict the salary best
#The best method is the one with the lowest Test MSE.
#If Lasso gives the lowest MSE, then some variables are likely irrelevant.
#If Ridge performs best, then most predictors contribute to the response.
#If Elastic Net performs best, then a combination of both strategies works well.
