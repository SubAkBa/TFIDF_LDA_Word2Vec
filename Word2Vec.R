install_github("bmschmidt/wordVectors")
install.packages("tsne")

library(devtools)
library(wordVectors)
library(tsne)

rm(list = ls()); gc(reset = T)

par(family = "AppleGothic")

dd <- read.csv("Digital_Daily_NLP4kec_rmNA.csv", stringsAsFactors = F)

# word2vec train용 txt파일
write.table(dd$content, file = "Digital_Daily.txt", row.names = F, col.names = F)

model <- train_word2vec("Digital_Daily.txt", output_file = "Digital_Daily.bin", 
                        threads = 2, vectors = 100, window = 5)
read.vectors("Digital_Daily.bin")

# 연관 키워드 추출하기
nearest_to(model, model[["디지털"]], 10)
project(model, model[["디지털"]])

# 2가지 이상 키워드에 대한 연관 키워드 추출하기
some <- nearest_to(model, model[[c("LG", "디지털", "KT", "데이터")]], 20)

# 단어간 연산하기
subVec <- model[rownames(model) == "디지털", ] - model[rownames(model) == "LG", ] +
  model[rownames(model) == "데이터", ]
subNear <- nearest_to(model, subVec, 20)
