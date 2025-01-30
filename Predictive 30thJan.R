library(MASS)
data=Boston
attach(Boston)
View(data)
?Boston
#1
class(data)
nrow(data)
ncol(data)
dim(data)
paste("Rows represent the various features")

df=data[,c('medv','crim','nox','black','lstat')]
df
X=df[,c('crim','nox','black','lstat')]
y=df$medv
ncol(X)
par(mfrow=c(2,2))
plot(X[,1],y,main = "crim vs medv")
plot(X[,2],y,main = "nox vs medv")
plot(X[,3],y,main = "black vs medv")
plot(X[,4],y,main = "lstat vs medv")

which(data$medv==min(data$medv))
X[which(df$medv==min(df$medv)),]

summary(df)
hist(lstat,freq = F)
lines(density(lstat))
par(mfrow=c(1,1))
which(df$crim>60)
which(tax>600)

sum(chas)

length(which(rm>7))
sum(rm>7)

#7
paste("out of all the features in the dataset which feature causes a drastic change in the tax rate of the houses")

cor(data,data$tax)
