library(tm)
library(NLP4kec)
library(dplyr)
library(plyr)
library(randomForest)

rm(list = ls()); gc(reset = T)

# 두산 백과의 Travel, Sport, Life 데이터(TF-IDF 이용)
# travel = 0, sport = 1, life = 2
travel <- read.csv("Dictionary_Travel.csv", stringsAsFactors = F) # 9840개
sport <- read.csv("Dictionary_Sport.csv", stringsAsFactors = F) # 7259개
life <- read.csv("Dictionary_Life.csv", stringsAsFactors = F) # 16291개
stopworddic <- read.csv("Stopword.csv", stringsAsFactors = F)

# 7000개씩 뽑아서 사용
travel <- travel[rownames(sample_n(travel, size = 7000)), ]
travel$title <- NULL
sport <- sport[rownames(sample_n(sport, size = 7000)), ]
sport$title <- NULL
life <- life[rownames(sample_n(life, size = 7000)), ]
life$title <- NULL

doosan_data <- data.frame()
doosan_data <- rbind(doosan_data, travel, sport, life)

# TF-IDF Document Term Matrix 생성
parsing_data <- r_parser_r(doosan_data$content, useEn = F, language = "ko", korDicPath = "mydictionary.txt")
parsing_data <- gsub(" ", "  ", parsing_data)
corpus_data <- VCorpus(VectorSource(parsing_data))
corpus_data <- tm_map(corpus_data, content_transformer(tolower))
corpus_data <- tm_map(corpus_data, removeWords, stopworddic$stopword)
dtm_data <- DocumentTermMatrix(corpus_data, control = list(removeNumbers = T, removePunctuation = T,
                                                           wordLengths = c(2, Inf),
                                                           weighting = function(x) weightTfIdf(x, normalize = T) ))
dtm_rmspar_data <- removeSparseTerms(dtm_data, sparse = as.numeric(0.98))
dtm_df_data <- as.data.frame(as.matrix(dtm_rmspar_data))

# label
label_df <- factor(c(rep("0", 7000), rep("1", 7000), rep("2", 6962)))
dtm_df_data$label <- label_df

# train / test로 나누기 - 70% / 30%
set.seed(1337)
tfidf_train <- data.frame()
tfidf_train_idx <- c()
idx <- c(0, 7000, 14000, 20962)
for(i in 1 : 3){
  tfidf_train_idx <- c(tfidf_train_idx, rownames(sample_frac(dtm_df_data[(idx[i] + 1) : idx[i + 1], ], 0.7)))
}
tfidf_train <- dtm_df_data[tfidf_train_idx, ]
tfidf_test <- dtm_df_data[-as.numeric(tfidf_train_idx), ]

# RandomForest
tfidf_train_rf <- randomForest(label ~ ., tfidf_train, importance = T, do.trace = T, ntree = 300)
print(tfidf_train_rf)
plot(tfidf_train_rf)
varImpPlot(tfidf_train_rf)

tfidf_predict_rf <- predict(tfidf_train_rf, newdata = tfidf_test, type = "class")
confusionMatrix(tfidf_predict_rf, tfidf_test$label)
