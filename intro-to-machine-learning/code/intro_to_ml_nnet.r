library(caret)
set.seed(42)
data = read.csv("datasets/normalized-college-data-set.csv", header=T)
index = createDataPartition(data$Private, p=0.70, list=FALSE)
train_data = data[index,]
test_data = data[-index,]

fitControl = trainControl(method="repeatedcv", number=5, repeats=3)

nn = train(as.factor(Private) ~ ., data=train_data, 
           method="nnet", trControl=fitControl, verbose=FALSE)


confusionMatrix(predict(nn, test_data), as.factor(test_data$Private))

# how would we do it with a random forest?
model_rf = train(as.factor(Private) ~ ., data=train_data, 
                 method="rf", trControl=fitControl, verbose=FALSE)
confusionMatrix(predict(model_rf, test_data), as.factor(test_data$Private))

# how would we do it with a decision tree?

# load needed libraries
library(rpart)
library(rpart.plot)

model_dt <- rpart(Private ~ ., data = train_data, method = "class", 
                  control = rpart.control(xval = 10, minbucket = 2))

#visualize the trained decision tree
rpart.plot(model_dt, extra = 100)

# predict testData with the decision tree
confusionMatrix(predict(model_dt, test_data, type = "class"), as.factor(test_data$Private))
