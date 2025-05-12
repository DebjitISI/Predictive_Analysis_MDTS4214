library(ISLR)
data("Default")
data = Default
attach(data)
View(data)
model = glm(default ~ income+balance, data, family = binomial(link = logit));model
pred = predict(model,data,type = "response"); pred
summary(pred)
#table(Default$default,pred)
pred=sort(pred,decreasing = T)
a=array(dim=1)
predict = matrix(data=NA,nrow=length(pred),ncol = length(pred),byrow=F)
for (i in 1:length(pred))
{
  for (j in 1:length(pred))
    predict[i,j] = ifelse(pred[j]>=pred[i],1,0)
}
predict

pred = sort(pred, decreasing = TRUE)

# Create a matrix of comparisons directly.
predict = as.integer(outer(pred, pred, ">="))

dim(predict)
predict
