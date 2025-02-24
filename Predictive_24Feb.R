data("Credit",package="ISLR")
attach(Credit)
View(Credit)
library(dplyr)
data= select(Credit,Balance,Limit,Age,Income)

y_bar=mean(data$Balance);y_bar

lm1= lm(Balance ~ Limit,data = data);lm1
summary(lm1)
lm2= lm(Balance ~ Age,data = data);lm2
summary(lm2)
lm3= lm(Balance ~ Income,data = data);lm3
summary(lm3)
lm12=lm(Balance ~ Limit + Age,data = data)
summary(lm12)
lm13=lm(Balance ~ Limit + Income,data = data)
summary(lm13)
lm132=lm(Balance ~ Limit + Income + Age,data = data)
summary(lm132)
lapply(names(data)[-1], function(var){
  lm(as.formula(paste("Balance ~",var)))
})
