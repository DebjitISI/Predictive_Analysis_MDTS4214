library(ISLR)
View(Default)
attach(Default)

##Fit Multiple logistic reg
m = glm(default ~ student + income + balance, family = binomial(link = "logit"))

library(stargazer)
stargazer(m, type = "html", out = "27_03.html")

#predict() gives the phat values
p = predict(m, type = "response"); p

for(i in 1:1){
  y.hat = ifelse(p>=p[i], 1, 0)
  t = table(y.hat, default)
  tpr[i] = t[2,2]/(t[1,2]+t[2,2])
  fpr[i] = t[2,1]/(t[1,1]+t[2,1])
  f[i] = tpr[i]*(1-fpr[i])
}

max(f)
##chose the optimal cutpoint
cut_point = p[f==max(f)]
cut_point
#predicted defaults
final_y_hat = ifelse(p>cut_point, 1, 0); final_y_hat
#confusion matrix
final_confusion_matrix = as.matrix(table(final_y_hat, default)); final_confusion_matrix
final_tpr = final_confusion_matrix[2,2]/(final_confusion_matrix[1,2] + final_confusion_matrix[2,2])
final_fpr = final_confusion_matrix[2,1]/(final_confusion_matrix[1,1]+ final_confusion_matrix[2,1])
final_tpr; final_fpr
##tpr = sensitivity = pi1
##specificity = pi0
specificity = final_confusion_matrix[1,1]/(final_confusion_matrix[1,1]+ final_confusion_matrix[2,1])
specificity


#prob of predicted default from the selected sample
sum(final_y_hat==1)/length(default)

