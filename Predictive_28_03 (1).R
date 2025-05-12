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
y_hat= matrix(as.numeric(outer(p,p,'>=')),nrow = length(p));y_hat
thresholds = sort(unique(p))
actual = ifelse(train_data$Direction == "Up", 1, 0)
for (i in 1:length(thresholds)) {
  threshold = thresholds[i]
  predicted = ifelse(p >= threshold, 1, 0)
  
  # Compute confusion matrix elements
  TP = sum(predicted == 1 & actual == 1)
  FP = sum(predicted == 1 & actual == 0)
  FN = sum(predicted == 0 & actual == 1)
  TN = sum(predicted == 0 & actual == 0)
  
  # Avoid division by zero
  TPR = ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  FPR = ifelse((FP + TN) == 0, 0, FP / (FP + TN))
  
  # Calculate metric
  f[i] = TPR * (1 - FPR)
}

f
best_threshold = thresholds[which.max(f)]
best_value = max(f)

cat("Best Threshold:", best_threshold, "\n")
cat("Max TPR*(1 - FPR):", best_value, "\n")
