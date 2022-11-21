ads = read.csv("C:/Users/akash/Downloads/Advertising.csv")

ads

dim(ads)
head(ads)

library(ggplot2)

ggplot()+geom_point(aes(x = ads$TV,y = ads$sales))

library(caTools)
split = sample.split(ads,SplitRatio = 0.7)
train <- subset(ads,split == "TRUE")
test <- subset(ads,split == "FALSE")

dim(train)
dim(test)

model_lr = lm(formula = sales ~ TV,data = ads)
coef(model_lr)

y_pred = predict(model_lr,newdata = test)
y_pred


# Visualising the Training set results
ggplot()+geom_point(aes(x = train$TV,y = train$sales),color = "red") +
geom_line(aes(x = train$TV,y = predict(model_lr,newdata = train)),color = "blue")


# Visualising the Test set results
ggplot()+geom_point(aes(x = test$TV,y = test$sales),color = "red") +
  geom_line(aes(x = test$TV,y = predict(model_lr,newdata = test)),color = "blue")
