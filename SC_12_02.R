library(ISLR)
View(Credit)
attach(Credit)

install.packages("car")

data = data.frame(Credit[,c(12,6,3,4)]); data
c_matrix = cor(data[,-1])
c_matrix

##In-built function
library(car)
lmfit = lm(Balance ~ Limit + Rating + Age, data = data)
vif(lmfit)

##R2
lm1 = lm(Age ~ Limit + Rating)
lm2 = lm(Limit ~ Age + Rating)
lm3 = lm(Rating ~ Age + Limit)

library(stargazer)
stargazer(lm1, lm2, lm3, type = "text", out = "vif.text")

##r2 value
#0.011  0.994     0.994    