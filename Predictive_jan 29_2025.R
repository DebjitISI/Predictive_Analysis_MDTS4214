library(MASS)
attach(Boston)
View(Boston)
data= Boston[,-4]
View(data)
training_sample= data[1:406, ]# rows should be selected randomly / "sample" function should be used
test_data= data[407:506, ]

linear_fit= lm(medv ~., training_sample);linear_fit
# "." represents all the responses togather excluding medv

library(stargazer)
stargazer(linear_fit, type = "text", title = "Regresion of house price on different predictors", dep.var.labels = "House price", out = "Regression.text")
# if nothing in mentioned 80% should be training data and remaining should be test data
# type should be selected as html for ease of work/ dep.var.labels -> dependent variable name/ out -> output file name
stargazer(linear_fit, type = "html", title = "Regresion of house price on different predictors", dep.var.labels = "House price", out = "Regression.html")

