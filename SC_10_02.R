library(ISLR)
attach(Credit)
plot(Limit, Age)
plot(Limit, Rating)
lmfit1 = lm(Balance ~ Age + Limit + Rating)
summary(lmfit1)
lmfit2 = lm(Balance ~ Age + Limit)
summary(lmfit2)


