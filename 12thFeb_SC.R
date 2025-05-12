install.packages("car")
library(car)
library(ISLR)
View(Credit)
attach(Credit)
data = data.frame(Credit[,c(12,6,3,4)])
data
c_matrix = cor(data[,-1])
c_matrix

# vif calculation using inbuilt function
library(car)
lmfit = lm(Balance ~ Limit + Rating + Age)
lmfit
vif(lmfit)

# calculate vif using R squared formula

lm1 = lm(Age ~ Limit + Rating)
lm2 = lm(Limit ~ Age + Rating)
lm3 = lm(Rating ~ Age + Limit)
summary(lm1)
library(stargazer)
stargazer(lm1,lm2,lm3,type="text",out="vif.text")

vif_age = 1/(1-(0.011))
vif_age

vif_limit = 1/(1-(0.994))
vif_limit

vif_rating = 1/(1-(0.994))
vif_rating

# Calculation of vif using ratio of variances

m1 = lm(Balance ~ Age + Limit + Rating)
m2 = lm(Balance ~ Age)
m3 = lm(Balance ~ Limit)
m4 = lm(Balance ~ Rating)

stargazer(m1,m2,m3,m4,type="text",out = "viff.text")
0.669
1.336
0.005
0.075

(0.669)^2/(1.336)^2
(0.063)^2/(0.005)^2
(0.940)^2/(0.075^2)

##cleaning
library(olsrr)

#influential plot
ols_plot_cooksd_chart(m1, threshold = 1)

i = cooks.distance(m1)
which(i>1)

##leverage
hii = ols_leverage(m1)
n = length(Credit)
p = 3
lo = (3*(p+1))/n
which(hii > lo)

##outlier
ols_plot_resid_stand(m1, threshold = 2.5)

o = rstandard(m1)
out = which(abs(o) > 2.5)

Credit[-out, ] #remove (similar do for other two if req)

