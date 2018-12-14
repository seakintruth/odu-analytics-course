install.packages('caret', dependencies = TRUE)
library(rpart)
library(rpart.plot)
library(caret)




bc_data = read.csv("../datasets/cancer_dataset.csv", header=T)
set.seed(42)
index = createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
train_data = bc_data[index, ]
test_data  = bc_data[-index, ]

fit <- rpart(classes ~ ., data = train_data, method = "class", control = rpart.control(xval = 10, minbucket = 2, cp = 0), parms = list(split = "information"))
rpart.plot(fit, extra = 100)

model_rf = caret::train(classes ~ ., 
						data = train_data, method = "rf", 
						preProcess = c("scale", "center"), 
						trControl = trainControl(method = "repeatedcv", 
						                         number = 5, repeats = 3, 
												 savePredictions = TRUE, verboseIter = FALSE)
					    )

#When you specify `savePredictions = TRUE`, you can access the cross-validation resuls with `model_rf$pred`.
save(model_rf, file = "../models/model_rf.RData")

load("../models/model_rf.RData")

model_rf

str(model_rf$finalModel$forest)
model_rf$finalModel$confusion

# estimate variable importance
imp = model_rf$finalModel$importance
imp[order(imp, decreasing = TRUE), ]
importance = varImp(model_rf, scale = TRUE)
plot(importance)


#predicting test data

confusionMatrix(predict(model_rf, test_data), as.factor(test_data$classes))

results = data.frame(actual = test_data$classes, predict(model_rf, test_data, type = "prob"))

results$prediction = ifelse(results$benign > 0.5, "benign", ifelse(results$malignant > 0.5, "malignant", NA))

results$correct = ifelse(results$actual == results$prediction, TRUE, FALSE)

ggplot(results, aes(x = prediction, fill = correct)) + geom_bar(position = "dodge")

