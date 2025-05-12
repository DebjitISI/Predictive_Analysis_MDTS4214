library(ISLR)
Credit
attach(Credit)
data = data.frame(Balance, Limit, Age, Income)
mean(Balance)
lm(Balance ~ Limit, data)
summary(lm(Balance ~ Age, data))
summary(lm(Balance ~ Income, data))

summary(lm(Balance ~ Limit + Age, data))

