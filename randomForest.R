data(iris)

str(iris)

dim(iris)

library(caTools)
library(randomForest)

split = sample.split(iris,SplitRatio = 0.7)
split

train <- subset(iris,split == "TRUE")
test <- subset(iris,split == "FALSE")

set.seed(120)

classifier = randomForest(x = train[-5],y = train$Species,ntree = 500)
classifier

y_pred = predict(classifier,newdata = test[-5])
y_pred

cm = table(test[,5],y_pred)
cm

plot(classifier)

importance(classifier)

varImpPlot(classifier)
