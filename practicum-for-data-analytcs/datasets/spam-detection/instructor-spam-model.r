library(tidyverse)
library(tm)
library(caret)
library(tidytext)

# set the random number seed for development
set.seed(42)

# read in the data
spam.data = read.csv("spam-detection.csv", header=T)

# change message from a factor to a character
spam.data = spam.data %>% mutate(message = as.character(spam.data$message))

# give dataset an ID column for each message
spam.data = spam.data %>% mutate(ID = seq.int(nrow(spam.data)))

# unnest all the tokens
spam.tokens = unnest_tokens(spam.data, output = word, input = message)

# filter out any text that contains a digit anywhere or is not of length 5
spam.tokens = spam.tokens %>% filter(!str_detect(word, "^\\D+$") | !str_detect(word, "\\w{5,}"))
spam.tokens = spam.tokens %>% anti_join(stop_words, by="word")

spam.tokens = spam.tokens %>% mutate(word = SnowballC::wordStem(word))

# create a document-term matrix with all features and tf weighting
spam.dtm = spam.tokens %>% count(ID, word)
spam.dtm = spam.dtm %>% cast_dtm(document = ID, term = word, value = n)

# remove any tokens that are missing from more than 99% of the documents in the corpus	   
spam.dtm = spam.dtm %>% removeSparseTerms(sparse = .99)

# at this point we have filtered out some of the original messages that have not met our guidelines
# (only contains strings with numbers or of length less than 5)
# so we need to figure out which messages are left, we can do this with group_by
# followed by a meaningless summarise. The goal here is to get the classificaiton
# labels for the messages left in our data set
classes.i.want = spam.tokens %>% group_by(ID, classification) %>% summarise(n=n())

# build the model data
model_data = cbind.data.frame(as.matrix(spam.dtm), as.factor(classes.i.want$classification))
number_of_cols = ncol(model_data)
colnames(model_data) = c(colnames(model_data)[1:(number_of_cols-1)], "classification")

# split train and test
index = createDataPartition(model_data$classification, p = 0.7, list = FALSE)
train_data = model_data[index, ]
test_data  = model_data[-index, ]

# this runs pretty quickly
spam.ham_rf = train(classification ~ ., data = train_data,  
                    method = "rf", ntree = 200, 
                    trControl = trainControl(method = "oob"))

# get the predictions to play with classification threshold
results = data.frame(actual = test_data$classification, predict(spam.ham_rf, test_data, type = "prob"))

# check if moving these around changes specifically try and see what happens if you 
# try results$ham == 1.0 and results$spam > 0
results$prediction = ifelse(results$ham >= 0.5, "ham", ifelse(results$spam >= 0.5, "spam", NA))

results$correct = ifelse(results$actual == results$prediction, TRUE, FALSE)

ggplot(results, aes(x = prediction, fill = correct)) + geom_bar(position = "dodge")
