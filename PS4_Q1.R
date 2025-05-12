library(ISLR)
View(Weekly)

d = Weekly
attach(d)
train_data = d[Year>=1990 & Year<=2008,]
test_data = d[Year>=2009 & Year<= 2010,]; test_data
View(train_data)
fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial(link="logit"), data = train_data );fit
summary(fit)
##Lag1 is significant at 5% level of significance, Volume is significant at 10% level of significance

##Predicting the probability of response given the predictor value(pi)
p=predict(fit,type="response", newdata = train_data);p

##Get the median values
med = apply(train_data[,-c(1,8,9)], MARGIN = 2, median)
#Just put in the values for e^(bo.hat + b1.hat*(lag1) + ....)

library(pROC)
rc = roc(as.factor(train_data$Direction), p, auc = TRUE)
rc
##ROC curve - using the training samples
c = coords(rc, "all", ret = c("specificity", "sensitivity")); c
tpr = c$sensitivity
fpr = 1 - c$specificity
plot(fpr, tpr, type = "l")
abline(a = 0, b = 1, col = "red")

p_test = predict(fit, type = "response", newdata = test_data);p_test
rc_test = roc(as.factor(test_data$Direction), p_test)
##Threshold -> maximizes the TPR(1-FPR)
cc = coords(rc_test, "all", ret = c("sensitivity", "specificity", "threshold"), transpose = FALSE)
TPR = cc$sensitivity
FPR = 1-cc$specificity
qant = TPR*(1-FPR);qant
opt = which(qant == max(qant))
best_row = cc[opt,]; best_row
pred_res = ifelse(p_test >= best_row$threshold,1,0); pred_res
confusion.matrix = table(Predicted = pred_res, Actual = test_data$Direction); confusion.matrix

##test error rate
(sum(confusion.matrix)-sum(diag(confusion.matrix)))/sum(confusion.matrix)