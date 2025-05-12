library(ISLR)
View(Weekly)

d = Weekly
attach(d)
train_data = d[Year>=1990 & Year<=2008,]
View(train_data)
fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial(link="logit"), data = train_data )

summary(fit)
library(stargazer)
stargazer(fit, type = "html", out = "ps3q1.html")

##Lag1 is significant at 5% level of significance, Volume is significant at 10% level of significance

p=predict(fit,type="response", newdata = train_data);p
f=numeric(length(p))
for (i in 1:length(p))
{
  #y_hat=numeric(length(p))
  y_hat = 0
  y_hat = ifelse(p >= p[i], 1, 0)
  t = as.matrix(table(y_hat,train_data$Direction))
  ##if the p[i] is very small, then all y_hat would be 1, so 0-row would not be created
  ##the confusion matrix and vice-a-versa for the scenario when p[i] will be very large.
  ##so to ensure that the dim(t) = 2x2 always halding this two extreme scenarios manually.
  # Ensure both classes (0,1) exist
  if (!"0" %in% rownames(t)) {
    t = rbind(t, "0" = c(0, 0))  # Add a row for class "0"
  }
  if (!"1" %in% rownames(t)) {
    t = rbind(t, "1" = c(0, 0))  # Add a row for class "1"
  }
  tpr[i] = t[2,2]/(t[1,2]+t[2,2])
  fpr[i] = t[2,1]/(t[1,1]+t[2,1])
  f[i] = tpr[i]*(1-fpr[i])
}

##chose the optimal cutpoint
cut_point = p[f==max(f)]
cut_point
final_y_hat = ifelse(p>cut_point, 1, 0); final_y_hat
final_confusion_matrix = as.matrix(table(final_y_hat, train_data$Direction)); final_confusion_matrix
final_tpr = final_confusion_matrix[2,2]/(final_confusion_matrix[1,2] + final_confusion_matrix[2,2])
final_fpr = final_confusion_matrix[2,1]/(final_confusion_matrix[1,1]+ final_confusion_matrix[2,1])
final_tpr; final_fpr
##tpr = sensitivity = pi1
##specificity = pi0
specificity = final_confusion_matrix[1,1]/(final_confusion_matrix[1,1]+ final_confusion_matrix[2,1])
specificity

##ROC
sorted_indices <- order(fpr)
plot(fpr[sorted_indices], tpr[sorted_indices], type="l", col="red", lwd=2)  # Line plot

abline(a = 0, b = 1, col = "blue", lty = 2)

##Using pROC library
library(pROC)

rc = roc(as.factor(train_data$Direction), p, auc = TRUE)
rc
plot(rc, main = "ROC curve for weekly data") #it matches with the one obtained manually above

##it would return the coordinates of the ROC curve
coords(rc, "best", best.method = "closest.topleft")
#it returns the threshold, specificity, sensitivity
coords = coords(rc, "best", best.method = "closest.topleft")
best_threshold = coords$threshold; best_threshold ##equivalent to cut_point
predicted_p = ifelse(p >= best_threshold, 1, 0); predicted_p #equivalent to final_y_hat
confusion.mat = table(predicted_p, train_data$Direction); confusion.mat



