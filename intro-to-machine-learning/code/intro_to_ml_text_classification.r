library(tidyverse) # for tidy data analysis
library(tidytext)
library(tm)
library(caret)

congress = read.csv("../datasets/congress_bills_dataset.csv", header=T)
congress = mutate(congress, text = as.character(text))

congress_tokens = unnest_tokens(congress, output = word, input = text)
# remove numbers
congress_tokens = filter(congress_tokens, !str_detect(word, "^[0-9]*$"))
# remove stop words
congress_tokens = anti_join(congress_tokens, stop_words)
# stem the words
congress_tokens = mutate(congress_tokens, word = SnowballC::wordStem(word))

 # create a document-term matrix with all features and tf weighting
 # you can ignore the warning
congress_dtm = count(congress_tokens, ID, word)
congress_dtm = cast_dtm(congress_dtm, document = ID, term = word, value = n)

# remove any tokens that are missing from more than 99% of the documents in the corpus	   
congress_dtm = removeSparseTerms(congress_dtm, sparse = .99)

#Before building a fancy schmancy statistical model, we can first investigate if there are 
# certain terms or tokens associated with each major topic category. 

#We can do this purely with tidytext tools: we directly calculate the tf-idf for each term 
# treating each major topic code as the document, rather than the individual bill. 



#To calculate tf-idf directly in the data frame, first we count() the frequency each token 
#appears in bills from each major topic code, then use bind_tf_idf() 
#to calculate the tf-idf for each token in each topic:
congress_tfidf = count(congress_tokens, major, word)
congress_tfidf =  bind_tf_idf(congress_tfidf, 
	                         term = word, 
							 document = major, 
							 n = n
							)

# sort the data frame 
plot_congress = arrange(congress_tfidf, desc(tf_idf))
# and convert word to a factor column
plot_congress = mutate(plot_congress, word = factor(word, levels = rev(unique(word))))

plot_congress = filter(plot_congress, major %in% c(1, 2, 3, 6))
plot_congress = mutate(plot_congress, major = factor(major, levels = c(1, 2, 3, 6), labels = c("Macroeconomics", "Civil Rights",
                                      "Health", "Education")))
plot_congress = group_by(plot_congress, major)
plot_congress = top_n(plot_congress, 10)
plot_congress = ungroup(plot_congress)

ggplot(plot_congress, aes(word, tf_idf)) + geom_col() + labs(x = NULL, y = "tf-idf") + facet_wrap(~major, scales = "free") + coord_flip()


model_data = cbind.data.frame(as.matrix(congress_dtm), as.factor(congress$major))
number_of_cols = ncol(model_data)
colnames(model_data) = c(colnames(model_data)[1:(number_of_cols-1)], "major")

index = createDataPartition(model_data$major, p = 0.7, list = FALSE)
train_data = model_data[index, ]
test_data  = model_data[-index, ]

# this runs quickly
congress_rf = train(major ~ ., data = train_data, 
	                method = "rf", ntree = 200, 
					trControl = trainControl(method = "oob"))

# this takes a long time to run but uses cross fold validation to the model
#congress_rf = train(major ~ ., data = train_data, method = "rf", ntree = 200, trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, savePredictions = TRUE, verboseIter = FALSE))
congress_rf$finalModel
confusionMatrix(predict(congress_rf, test_data), test_data$major)
  