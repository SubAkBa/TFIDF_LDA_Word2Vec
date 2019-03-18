library(tm)
library(NLP4kec)
library(dplyr)
library(caret)
library(randomForest)

rm(list = ls()); gc(reset = T)

# 두산 백과 데이터(TF-IDF 이용)
# travel = 0, sport = 1, life = 2, Animal = 3, Religion = 4, Philosophy = 5, PureScience = 6, SocialScience = 7,
# DescriptiveScience = 8
travel <- read.csv("Dictionary_Travel.csv", stringsAsFactors = F) # 9840개
sport <- read.csv("Dictionary_Sport.csv", stringsAsFactors = F) # 7259개
life <- read.csv("Dictionary_Life.csv", stringsAsFactors = F) # 16291개
animal <- read.csv("Dictionary_Animal.csv", stringsAsFactors = F) # 7470개
religion <- read.csv("Dictionary_Religion.csv", stringsAsFactors = F) # 9345개
philosophy <- read.csv("Dictionary_Philosophy.csv", stringsAsFactors = F) # 2820개
puresci <- read.csv("Dictionary_PureScience.csv", stringsAsFactors = F) # 17340개
socialsci <- read.csv("Dictionary_SocialScience.csv", stringsAsFactors = F) # 20010개
descsci <- read.csv("Dictionary_DescriptiveScience.csv", stringsAsFactors = F) # 12405개
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
label_df <- factor(c(rep("0", 7000), rep("1", 7000), rep("2", nrow(dtm_df_data) - 14000)))
dtm_df_data$label <- label_df

# train / test로 나누기 - 70% / 30%
set.seed(1337)
tfidf_train <- data.frame()
tfidf_train_idx <- c()
idx <- c(0, 7000, 14000, nrow(dtm_df_data))
for(i in 1 : 3){
  tfidf_train_idx <- c(tfidf_train_idx, rownames(sample_frac(dtm_df_data[(idx[i] + 1) : idx[i + 1], ], 0.7)))
}
tfidf_train <- dtm_df_data[tfidf_train_idx, ]
tfidf_test <- dtm_df_data[-as.numeric(tfidf_train_idx), ]

# RandomForest
tfidf_model_rf <- randomForest(label ~ ., tfidf_train, importance = T, do.trace = T, ntree = 200)
print(tfidf_model_rf)
plot(tfidf_model_rf)
varImpPlot(tfidf_model_rf)

# train 정확도 확인
tfidf_predict_rf_train <- predict(tfidf_model_rf, newdata = tfidf_train, type = "class")
confusionMatrix(tfidf_predict_rf_train, tfidf_train$label)

# test 정확도 확인
tfidf_predict_rf <- predict(tfidf_model_rf, newdata = tfidf_test, type = "class")
confusionMatrix(tfidf_predict_rf, tfidf_test$label)

# train데이터보다 test데이터가 정확도가 낮다. -> Overfitting이 발생한거 같음.
# 1. 데이터의 양을 늘려보기 -> 12000개
life <- read.csv("Dictionary_Life.csv", stringsAsFactors = F) # 16291개
puresci <- read.csv("Dictionary_PureScience.csv", stringsAsFactors = F) # 17340개
socialsci <- read.csv("Dictionary_SocialScience.csv", stringsAsFactors = F) # 20010개
descsci <- read.csv("Dictionary_DescriptiveScience.csv", stringsAsFactors = F) # 12405개
stopworddic <- read.csv("Stopword.csv", stringsAsFactors = F)

# life = 0, puresci = 1, socialsci = 2, descsci = 3
life <- life[rownames(sample_n(life, size = 12000)), ]
life$title <- NULL
puresci <- puresci[rownames(sample_n(puresci, size = 12000)), ]
puresci$title <- NULL
socialsci <- socialsci[rownames(sample_n(socialsci, size = 12000)), ]
socialsci$title <- NULL
descsci <- descsci[rownames(sample_n(descsci, size = 12000)), ]
descsci$title <- NULL

doosan_data <- data.frame()
doosan_data <- rbind(doosan_data, life, puresci, socialsci, descsci)

# label
label_df <- factor(c(rep("0", 12000), rep("1", 12000), rep("2", 12000), rep("3", nrow(dtm_df_data) - 36000)))
dtm_df_data$label <- label_df

# train / test로 나누기 - 70% / 30%
set.seed(1337)
tfidf_train <- data.frame()
tfidf_train_idx <- c()
idx <- c(0, 12000, 24000, 36000, nrow(dtm_df_data))
for(i in 1 : 4){
  tfidf_train_idx <- c(tfidf_train_idx, rownames(sample_frac(dtm_df_data[(idx[i] + 1) : idx[i + 1], ], 0.7)))
}
tfidf_train <- dtm_df_data[tfidf_train_idx, ]
tfidf_test <- dtm_df_data[-as.numeric(tfidf_train_idx), ]

# test 정확도가 더 떨어졌다 ... 왜지 ...

# 2. k-fold Cross-validation을 해보기
createFolds(tfidf_train$label, k = 10)
str(tfidf_train)
