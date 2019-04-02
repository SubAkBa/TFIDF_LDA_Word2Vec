library(NLP4kec)
library(data.table)
library(tm)
library(dplyr)
library(topicmodels)
library(randomForest)
library(caret)

# 1. Load Data
travel <- fread("Dictionary_Travel_utf.csv", data.table = F, stringsAsFactors = F) # 9840
animal <- fread("Dictionary_Animal_utf.csv", data.table = F, stringsAsFactors = F) # 7470
sport <- fread("Dictionary_Sport_utf.csv", data.table = F, stringsAsFactors = F) # 7259
religion <- fread("Dictionary_Religion_utf.csv", data.table = F, stringsAsFactors = F) # 9345
stopword <- fread("Stopword_utf.csv", data.table = F, stringsAsFactors = F, header = T)

# 2. Using Data size 7000
travel <- sample_n(travel, 7000)
travel$title <- NULL
animal <- sample_n(animal, 7000)
animal$title <- NULL
sport <- sample_n(sport, 7000)
sport$title <- NULL
religion <- sample_n(religion, 7000)
religion$title <- NULL

# 3. Unification
doosan_data <- data.frame()
doosan_data <- rbind(doosan_data, travel, animal, sport, religion)

# 4. Text Pre-Processing
parsing_data <- r_parser_r(doosan_data$content, useEn = T, language = "ko", korDicPath = "mydictionary.txt")
corpus_data <- VCorpus(VectorSource(parsing_data))
corpus_data <- tm_map(corpus_data, content_transformer(tolower))
corpus_data <- tm_map(corpus_data, removeWords, stopword$stopword)

# 5. Document-Term Matrix : Term Frequency(TF), Term Frequency-Inverse Document Frequency(TF-IDF)
tf_data <- DocumentTermMatrix(corpus_data, control = list(removePunctuation = T, removeNumbers = T,
                                                          wordsLengths = c(2, Inf)))
tf_data <- removeSparseTerms(tf_data, as.numeric(0.98))

tfidf_data <- DocumentTermMatrix(corpus_data, control = list(removePunctuation = T, removeNumbers = T,
                                                             wordsLengths = c(2, Inf),
                                                             weighting = function(x) weightTfIdf(x) ))
tfidf_data <- removeSparseTerms(tfidf_data, sparse = as.numeric(0.98))
tfidf_df_data <- as.data.frame(as.matrix(tfidf_data))

# 6. Latent Dirichlet Allocation(LDA) : rowtotal > 0
rowtotal <- apply(tf_data, 1, sum)
tf_data <- tf_data[rowtotal > 0, ]
lda_data <- LDA(tf_data, k = 10, control = list(seed = 1337))

# 6-1. Topics & Change Column names
topic_data <- as.data.frame(posterior(lda_data)$topics)
colnames(topic_data)[1 : 10] <- paste0("topic", 1 : 10)

# 7. Label
labels <- c(rep("0", 6500), rep("1", 6500), rep("2", 6500), rep("3", nrow(topic_data) - 19500))
topic_data$label <- as.factor(labels)
labels <- c(rep("0", 6500), rep("1", 6500), rep("2", 6500), rep("3", nrow(tfidf_df_data) - 19500))
tfidf_df_data$label <- as.factor(labels)

# 8. Divide Train & Test - 50% & 50%
tfidf_idx <- createDataPartition(tfidf_df_data$label, p = 0.5)$Resample1
tfidf_train <- tfidf_df_data[tfidf_idx, ]
tfidf_test <- tfidf_df_data[-tfidf_idx, ]

lda_idx <- createDataPartition(topic_data$label, p = 0.5)$Resample1
lda_train <- topic_data[lda_idx, ]
lda_test <- topic_data[-lda_idx, ]

# Learning Method - Labeling Data rate : 50%
# 9. Supervised Learning
# 9-1. TF-IDF (RandomForest)
tfidf_rf_model <- randomForest(label ~ ., data = tfidf_train, do.trace = T, importance = T, ntree = 400)
tfidf_pred <- as.data.frame(predict(tfidf_rf_model, newdata = tfidf_test, type = "prob"))
tfidf_pred$maxprob <- apply(tfidf_pred, 1, max)
tfidf_pred$label <- predict(tfidf_rf_model, newdata = tfidf_test, type = "class")

confusionMatrix(tfidf_pred$label, tfidf_test$label) # Accuracy : 0.6892

# 9-2. LDA (RandomForest)
lda_rf_model <- randomForest(label ~ ., data = lda_train, do.trace = T, importance = T, ntree = 400)
lda_pred <- as.data.frame(predict(lda_rf_model, newdata = lda_test, type = "prob"))
lda_pred$maxprob <- apply(lda_pred, 1, max)
lda_pred$label <- predict(lda_rf_model, newdata = lda_test, type = "class")

confusionMatrix(lda_pred$label, lda_test$label) # Accuracy : 0.6642

tfidf_pred_cut <- tfidf_pred[1 : nrow(lda_pred), ]

# 10. Self-Training
# 10-1. TF-IDF
for(i in 1 : nrow(tfidf_test)){
  tfidf_rf_model <- randomForest(label ~ ., data = tfidf_train, importance = T, ntree = 250)
  tfidf_pred <- predict(tfidf_rf_model, newdata = tfidf_test[i, ], type = "class")
  tfidf_result <- tfidf_test[i, ]
  tfidf_result$label <- tfidf_pred
  tfidf_train <- rbind(tfidf_train, tfidf_result)
}

# 10-2. LDA
for(i in 1 : nrow(lda_test)){
  lda_rf_model <- randomForest(label ~ ., data = lda_train, importance = T, ntree = 250)
  lda_pred <- predict(lda_rf_model, newdata = lda_test[i, ], type = "class")
  lda_result <- lda_test[i, ]
  lda_result$label <- lda_pred
  lda_train <- rbind(lda_train, lda_result)
}

# 11. Co-Training
for(i in 1 : nrow(tfidf_test)){
  tfidf_rf_model <- randomForest(label ~ ., data = tfidf_train, importance = T, ntree = 250)
  lda_rf_model <- randomForest(label ~ ., data = lda_train, importance = T, ntree = 250)
  
  tfidf_pred <- as.data.frame(predict(tfidf_rf_model, newdata = tfidf_test, type = "prob"))
  tfidf_pred$maxprob <- apply(tfidf_pred, 1, max)
  tfidf_pred$label <- predict(tfidf_rf_model, newdata = tfidf_test, type = "class")
  
  lda_pred <- as.data.frame(predict(lda_rf_model, newdata = lda_test, type = "prob"))
  lda_pred$maxprob <- apply(lda_pred, 1, max)
  lda_pred$label <- predict(lda_rf_model, newdata = lda_test, type = "class")
  
  pred_label <- ifelse(lda_pred$maxprob > tfidf_pred$maxprob, lda_pred$label, tfidf_pred$label)
  
  tfidf_result <- tfidf_test[i, ]
  tfidf_result$label <- pred_label
  tfidf_train <- rbind(tfidf_train, tfidf_result)
  
  lda_result <- lda_test[i, ]
  lda_result$label <- pred_label
  lda_train <- rbind(lda_train, lda_result)
}

confusionMatrix(tfidf_df_data$label, tfidf_train$label)
confusionMatrix(lda_train$label, topic_data$label)

# 12. Using Another Data
descsci <- fread("Dictionary_DescriptiveScience_utf.csv", data.table = F, stringsAsFactors = F) # 12405
puresci <- fread("Dictionary_PureScience_utf.csv", data.table = F, stringsAsFactors = F) # 17340
socisci <- fread("Dictionary_SocialScience_utf.csv", data.table = F, stringsAsFactors = F) # 20010

# 12-1. Using Data size 12000 / Unify All data
descsci <- sample_n(descsci, 12000)
descsci$title <- NULL
puresci <- sample_n(puresci, 12000)
puresci$title <- NULL
socisci <- sample_n(socisci, 12000)
socisci$title <- NULL

sci_data <- data.frame()
sci_data <- rbind(descsci, puresci, socisci)

# 12-2. Text Pre-Processing
parsing_sci <- r_parser_r(sci_data$content, useEn = T, language = "ko", korDicPath = "mydictionary.txt")
corpus_sci <- VCorpus(VectorSource(parsing_sci))
corpus_sci <- tm_map(corpus_sci, content_transformer(tolower))
corpus_sci <- tm_map(corpus_sci, removeWords, stopword$stopword)

# 12-3. Document-Term Matrix
# (1) TF
tf_sci <- DocumentTermMatrix(corpus_sci, control = list(removeNumbers = T, removePunctuation = T,
                                                        wordLengths = c(2, Inf)))
tf_sci <- removeSparseTerms(tf_sci, sparse = as.numeric(0.98))

# (2) TF-IDF
tfidf_sci <- DocumentTermMatrix(corpus_sci, control = list(removeNumbers = T, removePunctuation = T,
                                                           wordLengths = c(2, Inf),
                                                           weighting = function(x) weightTfIdf(x) ))
tfidf_sci <- removeSparseTerms(tfidf_sci, sparse = as.numeric(0.98))
tfidf_df_sci <- as.data.frame(as.matrix(tfidf_sci))

# 12-4. LDA
# (1) remove rowsum == 0
rowtotal <- apply(tf_sci, 1, sum)
tf_sci <- tf_sci[rowtotal > 0, ]

# (2) Perform LDA
lda_sci <- LDA(tf_sci, k = 10, control = list(seed = 1337))

# (3) Topics
topic_sci <- as.data.frame(posterior(lda_sci)$topics)
colnames(topic_sci)[1 : 10] <- paste0("topic", 1 : 10)

# 12-5. Label
tfidf_labels <- c(rep("0", 12000), rep("1", 12000), rep("2", nrow(tfidf_df_sci) - 24000))
tfidf_df_sci$label <- tfidf_labels
lda_labels <- c(rep("0", 12000), rep("1", 12000), rep("2", nrow(topic_sci) - 24000))
topic_sci$label <- lda_labels

# 12-6. Predict
tfidf_sci_pred <- as.data.frame(predict(tfidf_df_sci, newdata = tfidf_df_sci, type = "prob"))
tfidf_sci_pred$maxprob <- apply(tfidf_sci_pred, 1, max)
tfidf_sci_pred$label <- predict(tfidf_df_sci, newdata = tfidf_df_sci, type = "class")

lda_sci_pred <- as.data.frame(predict(lda_rf_model, newdata = topic_sci, type = "prob"))
lda_sci_pred$maxprob <- apply(lda_sci_pred, 1, max)
lda_sci_pred$label <- predict(lda_rf_model, newdata = topic_sci, type = "class")

# 12-7. Co-training
model <- ifelse(lda_sci_pred$maxprob > tfidf_sci_pred$maxprob, "LDA", 
                ifelse(lda_sci_pred$maxprob == tfidf_sci_pred$maxprob, "Equal", "TF-IDF"))
maxprob <- ifelse(lda_sci_pred$maxprob > tfidf_sci_pred$maxprob, lda_sci_pred$maxprob, tfidf_sci_pred$maxprob)
theotherprob <- ifelse(lda_sci_pred$maxprob > tfidf_pred_cut$maxprob, tfidf_sci_pred$maxprob, lda_sci_pred$maxprob)
label <- ifelse(lda_sci_pred$maxprob > tfidf_pred_cut$maxprob, lda_sci_pred$label, tfidf_sci_pred$label)
result <- data.frame(model, maxprob, theotherprob, label)
