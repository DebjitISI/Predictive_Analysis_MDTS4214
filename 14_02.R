#8.
library(MASS)
attach(fgl)

library(stargazer)

View(fgl)
d = data.frame(fgl[type == "Veh",])
m = lm(RI ~ Na + Mg + Al + Si + K + Ca + Ba + Fe, data = d)
summary(m)

stargazer(m, type = "html", 
          title = "Regression of RI on metal oxides", 
          dep.var.labels = "RI", 
          notes = c("Data as been taken from R"), 
          out = "regression_fgl.html")

m1 = lm(RI ~ Fe, data = d)
summary(m1)

m2 = lm(RI ~ Fe + I(Fe^2), data = d)
summary(m2)

m3 = lm(RI ~ Fe + I(Fe^2) + I(Fe^3), data = d)
summary(m3)

m4 = lm(RI ~ Fe + I(Fe^2) + I(Fe^3) + I(Fe^4), data = d)
summary(m4)

plot(Fe, RI, type = "p")

##9.
library(ISLR)
attach(Credit)

plot(Age, Limit, type = "p", pch = 16)
plot(Rating, Limit, type = "p", pch = 16)

lmf1 = lm(Balance ~ Age + Limit)
lmf2 = lm(Balance ~ Age + Limit + Rating)
lmf3 = lm(Balance ~ Rating + Limit)

stargazer(lmf1, lmf2, lmf3, type = "html", 
          title = "Regression of Balance on Age, Limit, Rating", 
          dep.var.labels = "Balance", 
          notes = c("Data as been taken from R"), 
          out = "regression_Credit.html")

library(car)
vif(lmf2)
vif(lmf1)
vif(lmf3)
