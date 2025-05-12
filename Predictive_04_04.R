set.seed(seed = 123)
d = read.csv("C:/Users/AG/Downloads/data.csv")
View(d)

##Drop the columns - use only those cols required for model fiting
data = d[,2:14]
View(data)
##Now drop the rows which have at atleast one NA value
data = na.omit(data)
View(data)
dim(data)

##Defining the response and predictor
y = data$m12; length(y)
x = data[,2:13]; dim(x)

##splitting the data
index = sample(1:nrow(data), nrow(data), replace = FALSE); index
train.index = index[1:(0.8*nrow(data))]; train.index
train = data[train.index,]; train
test = data[-train.index,]; test
train; test
View(train); View(test)
dim(train); dim(test)

##Fitting the model
train$m12 = relevel(as.factor(train$m12), ref = "3")
library(nnet)
fit = multinom(m12 ~ as.factor(m5) + as.factor(m6) + as.factor(c4) + as.factor(c5) + 
                  as.factor(m2) + m3 + as.factor(c1) + as.factor(c2) + as.factor(c3) + 
                  as.factor(c7) + as.factor(c8) + c9, data = train)
summary(fit)
##predict on test data
p = predict(fit, newdata = test)

library(stargazer)
stargazer(fit, type = "html", out = "04_04.html")



##Confusion matrix
t = table(test$m12,p);t

##test error = sum of off diagonals/total
##diagonals = total number of correct predictions
##sum(t) = total number of predictions
(sum(t) - sum(diag(t)))/sum(t)
