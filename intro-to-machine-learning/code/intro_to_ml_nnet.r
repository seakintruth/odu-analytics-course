library(caret)
set.seed(42)
data = read.csv("../datasets/normalized-college-data-set.csv", header=T)
idx = createDataPartition(data$Private, p=0.75, list=FALSE)
train = data[idx,]
test = data[-idx,]

fitControl = trainControl(method="repeatedcv", number=5, repeats=3)

nn = train(factor(Private) ~ ., data=train, method="nnet", trControl=fitControl, verbose=FALSE)

predicted.nn.values = predict(nn, test[2:18])

print(head(predicted.nn.values, 3))

table(test$Private, predicted.nn.values)