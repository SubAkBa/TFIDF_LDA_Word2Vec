library(e1071)
library(NLP4kec)

travel <- read.csv("Dictionary_Travel.csv", stringsAsFactors = F)
sport <- read.csv("Dictionary_Sport.csv", stringsAsFactors = F)
life <- read.csv("Dictionary_Life.csv", stringsAsFactors = F)
stopwords <- read.csv("StopWord.csv", stringsAsFactors = F)


travel <- travel[rownames(sample_n(travel, size = 7000)), ]
travel$title <- NULL
sport <- sport[rownames(sample_n(sport, size = 7000)), ]
sport$title <- NULL
life <- life[rownames(sample_n(life, size = 7000)), ]
life$title <- NULL

doosan_data <- data.frame()
doosan_data <- rbind(doosan_data, travel, sport, life)

parsing_doosan <- r_parser_r(doosan_data$content, useEn = T, language = "ko", korDicPath = "mydictionary.txt")
parsing_doosan <- gsub(" ", "  ", parsing_doosan)
corpus_doosan <- VCorpus(VectorSource(parsing_doosan))
corpus_doosan <- tm_map(corpus_doosan, content_transformer(tolower))
corpus_doosan <- tm_map(corpus_doosan, removeWords, stopwords$stopword)
dtm_doosan <- DocumentTermMatrix(corpus_doosan, control = list(removePunctuation = T, removeNumbers = T,
                                                               wordLengths = c(2, Inf)))
dtm_doosan <- removeSparseTerms(dtm_doosan, sparse = as.numeric(0.98))

rowtotal <- apply(dtm_doosan, 1, sum)
dtm_doosan <- dtm_doosan[rowtotal > 0, ]

# LDA
lda_doosan <- LDA(dtm_doosan, k = 10, control = list(seed = 1234))
lda_doosan <- LDA(dtm_doosan, k = 20, control = list(seed = 1234))

topic_doosan <- topics(lda_doosan, 1)
topic_doosan_df <- as.data.frame(topic_doosan)
topic_doosan_df$row <- as.numeric(row.names(topic_doosan_df))

doc_prob_doosan <- posterior(lda_doosan)$topics
doc_prob_doosan_df <- as.data.frame(doc_prob_doosan)

doc_prob_doosan_df$row <- as.numeric(row.names(doc_prob_doosan_df))
id_topic_doosan <- merge(topic_doosan_df, doc_prob_doosan_df, by = "row")

label_df <- factor(c(rep("1", 7000), rep("2", 7000), rep("3", NROW(doc_prob_doosan_df) - 14000)))
doc_prob_doosan_df$label <- label_df

colnames(doc_prob_doosan_df)[11 : 20] <- paste0("topic", colnames(doc_prob_doosan_df)[11 : 20])
doc_prob_doosan_df$row <- NULL

# Divide train / test data
set.seed(1337)
lda_train <- data.frame()
lda_train_idx <- c()
idx <- c(0, 7000, 14000, nrow(doc_prob_doosan_df))
for(i in 1 : 3){
  lda_train_idx <- c(lda_train_idx, rownames(sample_frac(doc_prob_doosan_df[(idx[i] + 1) : idx[i + 1], ], 0.7)))
}
lda_train <- doc_prob_doosan_df[lda_train_idx, ]
lda_test <- doc_prob_doosan_df[-as.numeric(lda_train_idx), ]

# Naive Bayesian
lda_model_nb <- naiveBayes(label ~ ., data = lda_train)
lda_train_pred_nb <- predict(lda_model_nb, newdata = lda_train, type = "class")
confusionMatrix(lda_train_pred_nb, lda_train$label) # Train - Accuracy : 0.88

lda_test_pred_nb <- predict(lda_model_nb, newdata = lda_test, type = "class")
confusionMatrix(lda_test_pred_nb, lda_test$label) # Test - Accuracy : 0.8773


# Q1 : Topic의 갯수는 어떠한 기준으로 정하는가?
# Q2 : TF-IDF의 경우 모델의 독립변수는 Terms = 단어들 / LDA의 경우 모델의 독립변수는 생성되는 Topics = 주제들이다.
#      논문에서는 똑같은 독립변수를 집어넣는다 .....