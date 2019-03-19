install.packages("tidytext")
install.packages("topicmodels")

library(ggplot2)
library(dplyr)
library(tidyr)
library(NLP4kec)
library(tm)
library(tidytext)
library(topicmodels)
library(caret)
library(randomForest)

rm(list = ls()); gc(reset = T)

dd <- read.csv("Digital_Daily_NLP4kec_rmNA.csv", stringsAsFactors = F)
parsing <- r_parser_r(dd$content, useEn = T, language = "ko", korDicPath = "mydictionary.txt")
parsing <- gsub(" ", "  ", parsing)
corpus <- VCorpus(VectorSource(parsing))
corpus <- tm_map(corpus, content_transformer(tolower))
dtm <- DocumentTermMatrix(corpus, control = list(removePunctuation = T, removeNumbers = T, wordLengths = c(2, Inf)))
# dtm <- DocumentTermMatrix(corpus, control = list(removePunctuation = T, removeNumbers = T, wordLengths = c(2, Inf),
                                                 # weighting = function(x) weightTfIdf(x) ))
                                                 # Error : The DocumentTermMatrix needs to have a term frequency weighting
dtm <- removeSparseTerms(dtm, as.numeric(0.9))

rowtotal <- apply(dtm, 1, sum) # Error : Each row of the input matrix needs to contain 
dtm <- dtm[rowtotal > 0, ]     #         at least one non-zero entry.

lda <- LDA(dtm, k = 2, control = list(seed = 1234)) # k : topic 갯수

# Word-topic probabilities
topics <- tidy(lda, matrix = "beta") # ex) ai가 topic1에서 출현할 확률(beta) = 0.00530
top_terms <- topics %>% group_by(topic) %>% 
  top_n(10, beta) %>% ungroup() %>% arrange(topic, desc(beta))
top_terms %>% ggplot(aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# A slightly diffferent approach is to determine the terms 
# that have the greatest difference in Beta 
# between Group 1 and Group 2 by calculating log2(beta2 / beta1)
beta_spread <- topics %>% mutate(topic = paste0("topic", topic)) %>% 
  spread(topic, beta) %>% filter(topic1 > .001 | topic2 > .001) %>% 
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(15, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

# Document-topic probabilities
documents <- tidy(lda, matrix = "gamma") # ex) 문서1내에서 topic1로 부터 52.3%만큼 단어들이 생성됐다.


# 두산 백과
rowtotal <- apply(dic_dtm, 1, sum)
dic_dtm <- dic_dtm[rowtotal > 0, ]
lda <- LDA(dic_dtm, k = 2, control = list(seed = 1234))

# 단어-주제 확률
dic_topic <- tidy(lda, matrix = "beta")
top_terms <- dic_topic %>% group_by(topic) %>% 
  top_n(10, beta) %>% ungroup() %>% arrange(topic, desc(beta))
top_terms %>% ggplot(aes(x = reorder(term, beta), y = beta, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") + coord_flip()


# 두산 백과 
travel <- read.csv("Dictionary_Travel.csv", stringsAsFactors = F)
sport <- read.csv("Dictionary_Sport.csv", stringsAsFactors = F)
life <- read.csv("Dictionary_Life.csv", stringsAsFactors = F)
stopwords <- read.csv("StopWord.csv", stringsAsFactors = F)

# 7000개씩 뽑아서 사용
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

dtm_doosan_tfidf <- DocumentTermMatrix(corpus_doosan, control = list(removePunctuation = T, removeNumbers = T,
                                                                     wordLengths = c(2, Inf),
                                                                     weighting = function(x) weightTfIdf(x) ))
lda_doosan_tfidf <- LDA(dtm_doosan_tfidf, k = 3, control = list(seed = 1234))

# 문서별 토픽번호 저장
lda_doosan <- LDA(dtm_doosan, k = 10, control = list(seed = 1234))
lda_doosan <- LDA(dtm_doosan, k = 20, control = list(seed = 1234))
topic_doosan <- topics(lda_doosan, 1)
topic_doosan_df <- as.data.frame(topic_doosan)
topic_doosan_df$row <- as.numeric(row.names(topic_doosan_df))

# 토픽별 핵심단어 저장
term_topic_doosan <- terms(lda_doosan, 30)

# 문서별 토픽확률값 저장
doc_prob_doosan <- posterior(lda_doosan)$topics
doc_prob_doosan_df <- as.data.frame(doc_prob_doosan)

doc_prob_doosan_df$maxprob <- apply(doc_prob_doosan_df, 1, max)

# 문서별 토픽번호 및 확률값 출력
doc_prob_doosan_df$row <- as.numeric(row.names(doc_prob_doosan_df))
id_topic_doosan <- merge(topic_doosan_df, doc_prob_doosan_df, by = "row")

label_df <- factor(c(rep("1", 7000), rep("2", 7000), rep("3", NROW(doc_prob_doosan_df) - 14000)))
doc_prob_doosan_df$label <- label_df

colnames(doc_prob_doosan_df)[1 : 10] <- paste0("topic", colnames(doc_prob_doosan_df)[1 : 10])

# train / test로 나누기 - 70% / 30%
set.seed(1337)
lda_train <- data.frame()
lda_train_idx <- c()
idx <- c(0, 7000, 14000, nrow(doc_prob_doosan_df))
for(i in 1 : 3){
  lda_train_idx <- c(lda_train_idx, rownames(sample_frac(doc_prob_doosan_df[(idx[i] + 1) : idx[i + 1], ], 0.7)))
}
lda_train <- doc_prob_doosan_df[lda_train_idx, ]
lda_test <- doc_prob_doosan_df[-as.numeric(lda_train_idx), ]

# RandomForest
rf_lda_train <- randomForest(label ~ ., lda_train, importance = T, do.trace = T, ntree = 200)

print(rf_lda_train)
plot(rf_lda_train)
varImpPlot(rf_lda_train)

rf_lda_pred <- predict(rf_lda_train, newdata = lda_test, type = "class")
confusionMatrix(rf_lda_pred, lda_test$label) # topic 10개 - Accuracy : 0.9693 topic 20개 - ?

# LDA Output 4가지 종류
# 1. 토픽 별 핵심 단어 출력하기   : terms(lda, 30)
# 2. 문서 별 토픽 번호 출력하기   : topics(lda, 1)
# 3. 문서 별 토픽 확률값 출력하기 : posterior(lda)$topics
# 4. 단어 별 토픽 확률값 출력하기 : posterior(lda)$terms