library(MASS)
attach(Boston)
View(Boston)

#1.
class(Boston)
dim(Boston)
#rows representing each individual and col representing variable

#2.
data_B = Boston[,c(14,1,5,12,13)]
View(data_B)

par(mfrow = c(2,2))
plot(crim, medv, type = "p", pch = 20)
plot(nox, medv, type = "p", pch = 20)
plot(black, medv, type = "p", pch = 20)
plot(lstat, medv, type = "p", pch = 20)

#3.
which.min(medv)
which(data_B[,1] == min(data_B[,1])) #399, 406

data_B[which(data_B[,1] == min(data_B[,1])), -1]

summary(data_B)

#4.
plot(crim, type = "p")
which(data_B[,2] > 60)

plot(tax, type = "p")
which(tax > 700)

#<complete this>

#5.
sum(chas)

#6.
length(which(rm > 7))
sum(rm > 7)