# Naive Bayesian 사용

library(tm)
library(NLP4kec)
library(dplyr)
library(caret)
library(e1071)

travel <- read.csv("Dictionary_Travel.csv", stringsAsFactors = F) # 9840개
sport <- read.csv("Dictionary_Sport.csv", stringsAsFactors = F) # 7259개
life <- read.csv("Dictionary_Life.csv", stringsAsFactors = F) # 16291개
stopword <- read.csv("Stopword.csv", stringsAsFactors = F)

travel <- travel[rownames(sample_n(travel, size = 7000)), ]
travel$title <- NULL
sport <- sport[rownames(sample_n(sport, size = 7000)), ]
sport$title <- NULL
life <- life[rownames(sample_n(life, size = 7000)), ]
life$title <- NULL

doosan_data <- data.frame()
doosan_data <- rbind(doosan_data, travel, sport, life)

parsing_data <- r_parser_r(doosan_data$content, useEn = F, language = "ko", korDicPath = "mydictionary.txt")
parsing_data <- gsub(" ", "  ", parsing_data)
corpus_data <- VCorpus(VectorSource(parsing_data))
corpus_data <- tm_map(corpus_data, content_transformer(tolower))
corpus_data <- tm_map(corpus_data, removeWords, stopword$stopword)
dtm_data <- DocumentTermMatrix(corpus_data, control = list(removeNumbers = T, removePunctuation = T,
                                                           wordLengths = c(2, Inf),
                                                           weighting = function(x) weightTfIdf(x, normalize = T) ))
dtm_rmspar_data <- removeSparseTerms(dtm_data, sparse = as.numeric(0.98))
dtm_df_data <- as.data.frame(as.matrix(dtm_rmspar_data))


label_df <- factor(c(rep("0", 7000), rep("1", 7000), rep("2", nrow(dtm_df_data) - 14000)))
dtm_df_data$label <- label_df


set.seed(1337)
tfidf_train <- data.frame()
tfidf_train_idx <- c()
idx <- c(0, 7000, 14000, nrow(dtm_df_data))
for(i in 1 : 3){
  tfidf_train_idx <- c(tfidf_train_idx, rownames(sample_frac(dtm_df_data[(idx[i] + 1) : idx[i + 1], ], 0.7)))
}
tfidf_train <- dtm_df_data[tfidf_train_idx, ]
tfidf_test <- dtm_df_data[-as.numeric(tfidf_train_idx), ]

# 나이브 베이지안 분류기
tfidf_model_nb <- naiveBayes(label ~ ., data = tfidf_train)
tfidf_model_nb_lap <- naiveBayes(label ~ ., data = tfidf_train, laplace = as.numeric(label))

tfidf_predict_nb <- predict(tfidf_model_nb, newdata = tfidf_train, type = "class")
confusionMatrix(tfidf_predict_nb, tfidf_train$label) # Train - Accuracy : 0.8093

tfidf_predict_nb_lap <- predict(tfidf_model_nb_lap, newdata = tfidf_train, type = "class")
confusionMatrix(tfidf_predict_nb_lap, tfidf_train$label) # Train + laplace - Accuracy : 0.8093

tfidf_predict_nb_test <- predict(tfidf_model_nb, newdata = tfidf_test, type = "class")
confusionMatrix(tfidf_predict_nb_test, tfidf_test$label) # Test - Accuracy : 0.803

tfidf_predict_nb_test_lap <- predict(tfidf_model_nb_lap, newdata = tfidf_test, type = "class")
confusionMatrix(tfidf_predict_nb_test_lap, tfidf_test$label) # Test + laplace - Accuracy : 0.803
