install.packages("KoNLP", dependencies = T)
install.packages("digest")
install.packages("rlang")
install.packages("stringi")
install_github("NamyounKim/NLP4kec")
install.packages("rJava")
install.packages("D:/Analysis_Data/TFIDF_LDA_Word2Vec/NLP4kec_1.2.0.zip", repos = NULL)
install.packages("network")
install.packages("sna")
install.packages("GGally")
install.packages("qgraph")

library(KoNLP)
library(rlang)
library(digest)
library(stringi)
library(devtools)
library(tm)
library(rJava)
library(NLP4kec)
library(dplyr)

library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

library(qgraph)

library(randomForest)

rm(list = ls()); gc(reset = T)

# KoNLP Library : https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md
# NLP4kec Library : https://github.com/NamyounKim/NLP4kec

useSejongDic() # 형태소 분석 하기 위해서는 reference로 삼을 사전이 필요하다.
               # 시스템(system) / 세종(sejong) 사전이 있다.
# 예시 1
sentence <- "아버지가 방에 스르륵 들어가신다."

# extractNoun() : 명사 추출
extractNoun(sentence)
extractNoun(dd$abody[1 : 5])

# mergeUserDic() : 사전에 단어를 추가
# '스르륵'은 명사가 아닌 부사
mergeUserDic(data.frame(c("스르륵"), c("mag"))) # data.frame의 형태로 추가해야한다.
                                                # 중요도가 떨어졌다. (buildDictionary() 사용)
buildDictionary(ext_dic = "woorimalsam", user_dic = data.frame("스르륵", "mag"))
extractNoun(sentence) # 스르륵은 이제 명사로 추출되지 않는다.

# MorphAnalyzer : 형태소 분석
MorphAnalyzer(sentence)

# SimplePos09 : 9개의 품사 태그를 달아 준다.
# SimplePos22 : 22개의 품사 태그를 달아 준다.
SimplePos09(sentence)

# library(tm)
# tm_map(corpus. tolower) : 소문자로 만들기
# tm_map(corpus, stemDocument) : 어근만 남기기
# tm_map(corpus, stripWhitespace) : 공백제거
# tm_map(corpus, removePunctuation) : 문장부호 제거
# tm_map(corpus, removeNumbers) : 숫자 제거
# tm_map(corpus, removeWords, “word”) : 단어 제거
# tm_map(corpus, remobeWords, stopwords(“english”)) : 불용어 제거
# tm_map(corpus, PlainTextDocument) : TextDocument로 변환

# 디지털 데일리 기사
dd <- read.csv("Digital_Daily.csv", stringsAsFactors = F)
# <U+00A0> -> 줄바꿈 없는 공백(NBSP), 단어 잘림 방지 공백, 줄 바꿈하지않는 공백 : 현 위치에서 자동 줄 바꿈을 막는데 쓰인다.
dd$abody <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", dd$abody) # [:alnum:] : Alphanumeric characters(0-9 a-Z)
dd$abody <- gsub("U00A0", " ", dd$abody)                      # [:blank:] : spaces and tabs
                                                     # ?&/\\- Specific characters you want to save for some reason. 
                                                     #        Punctuation signs can be saved here.
write.csv(dd, "Digital_Daily.csv", row.names = F)

MorphAnalyzer(dd$abody[1])
SimplePos22(dd$abody[1])
corpus_test <- Corpus(VectorSource(dd$abody[1]))
corpus_test <- tm_map(corpus_test, removeNumbers)
corpus_test <- tm_map(corpus_test, tolower)

# Add doc_id
write.csv(dd, "Digital_Daily.csv")
dd <- read.csv("Digital_Daily.csv", stringsAsFactors = F)
colnames(dd)[1] <- "id"

corpus_test <- DataframeSource(data.frame(doc_id = dd$id, text = dd$abody))
corpus <- Corpus(corpus_test)
inspect(corpus)
dtm <- DocumentTermMatrix(corpus)
inspect(dtm)
df <- cbind(as.data.frame(as.matrix(dtm)),LABEL=rep("diary",length(corpus)))

corp <- VCorpus(VectorSource(dd$abody[1 : 5]))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
dtm <- DocumentTermMatrix(corp, control = list(removeNumbers = T, wordLengths = c(2, Inf)))
dtm_test <- removeSparseTerms(dtm, as.numeric(0.97))
dtm_test # Non- / sparse entries : 0이 아닌 셀의 수 / 0인 셀의 수
         # Sparsity : 전체 셀 수에서 0인 셀의 비율
         # Maximal term length : 가장 긴 단어의 길이
         # Weighting : 가중치 종류 tf / tfidf

# library(NLP4kec)
# 읽어오기 위해서는 column name이 id / content로 되어 있어야 한다.
dd <- read.csv("Digital_Daily.csv", stringsAsFactors = F)
colnames(dd)[c(1, 5)] <- c("id", "content")
dd[, c(2 : 4)] <- NULL
dd$ID <- paste0("D", dd$ID)
write.csv(dd, "Digital_Daily_NLP4kec.csv", row.names = F)

result <- file_parser_r(path = "Digital_Daily_NLP4kec.csv", language = "ko") # 분석이 되지 않음.

dd <- read.csv("Digital_Daily_NLP4kec.csv", stringsAsFactors = F)
result <- r_parser_r(dd$content[1 : 5], language = "ko")
result <- gsub(" ","  ", result) # 단어 간 띄어쓰기 하나 더추가(윈도우만)

corp <- VCorpus(VectorSource(result)) # Corpus 생성
corp <- tm_map(corp, removePunctuation) # 특수문자 제거

# Document Term Matrix 생성
dtm <- DocumentTermMatrix(corp, control = list(removeNumbers = FALSE, wordLengths = c(2, Inf))) 

# 단어 양옆 스페이스 제거 및 한글자 단어 제외하기
colnames(dtm) <- trimws(colnames(dtm))  # 윈도우에서 돌리는 경우에만 적용
dtm <- dtm[, nchar(colnames(dtm)) > 1]

# 연관 키워드 구하기
findAssocs(dtm, terms = "데이터", corlimit = 0.2)

# 사용자 사전 적용해서 형태소 분석 할 것. korDicPath = "" / dictionary.txt (한줄 한줄 마다 단어 배치)
corpus <- r_parser_r(dd$content[1], language = "ko", useEn = T, korDicPath = "mydictionary.txt")
corpus <- gsub(" ", "  ", corpus); corpus

table(is.na(dd$content)) # NA 제거
dd <- dd %>% filter(!is.na(content))
write.csv(dd, "Digital_Daily_NLP4kec_rmNA.csv", row.names = F)
dd <- read.csv("Digital_Daily_NLP4kec_rmNA.csv", stringsAsFactors = F)
result <- r_parser_r(dd$content[1 : 100], language = "ko", korDicPath = "mydictionary.txt")
article_100 <- data.frame(content = dd$content[1 : 100], parser = result)
article_100[24, ]

# index 37까지 확인
index <- 29

result <- r_parser_r(dd$content[index], language = "ko", useEn = T, korDicPath = "mydictionary.txt")
article_100 <- data.frame(content = dd$content[index], parser = result)
article_100
index <- index + 1

# Document Term Matrix 생성
parser1 <- r_parser_r(dd$content, language = "ko", useEn = T, korDicPath = "mydictionary.txt")
parser1 <- gsub(" ", "  ", parser1)
corpus1 <- VCorpus(VectorSource(parser1))
corpus1 <- tm_map(corpus1, content_transformer(tolower)) # corpus[[index]]$content : Corpus 확인
                           # tm_map(corpus, PlainTextDocument)
dtm1 <- DocumentTermMatrix(corpus1,
                           control = list(removePunctuation = T, removeNumbers = T, wordLengthes = c(2, Inf)))
dtm1_sparse <- removeSparseTerms(dtm1, sparse = as.numeric(0.9))
inspect(dtm1_sparse) # 전체 매트릭스 확인 : View(t(as.matrix(dtm)))
findAssocs(dtm1_sparse, "글로벌", 0.2) # tf 가중치 사용 "글로벌" 연관 키워드 검색

# 가중치를 tf -> tfidf로 변경해보기.
dtm2 <- DocumentTermMatrix(corpus1, 
                           control = list(removePunctuation = T, removeNumbers = T, wordLengths = c(2, Inf),
                                          weighting = function(x) weightTfIdf(x, normalize = T)) )
colnames(dtm2) <- trimws(colnames(dtm2))
dtm2 <- dtm2[, nchar(colnames(dtm2)) > 1]
dtm2_sparse <- removeSparseTerms(dtm2, sparse = as.numeric(0.95))
inspect(dtm2_sparse)
findAssocs(dtm2_sparse, "글로벌", 0.2) # tfidf 가중치 사용 "글로벌" 연관 키워드
                                       # w(x,y) = tf(x, y) * log(N / df(x))
                                       # tf(x, y) : y문서에 x단어의 빈도수
                                       # df(x) : x단어가 포함된 문서수
                                       # N : 총 문서수

# 자주 출현하는 단어 찾기 : findFreqTerms(DTM, lowfreq = 0, hightfreq = Inf)
findFreqTerms(dtm2, lowfreq = 2)

# 연관 키워드 네트워크 맵
# (1) Make Cor Matrix
dtm2_matrix <- as.matrix(dtm2_sparse)
cor_dtm <- cor(dtm2_matrix)
cor_dtm[cor_dtm < 0.25] <- 0

# (2) Construct Network
net <- network(cor_dtm, directed = F)
net %v% "mode" <- ifelse(betweenness(net) > mean(betweenness(net)), "big", "small")
col <- c("small" = "grey", "big" = "gold")
set.edge.value(net, "edgeSize", cor_dtm * 2)

ggnet2(net, label = T, label.size = 3, color = "mode", palette = col, size = "degree", edge.size = "edgeSize",
       family = "ApplyGothic")

# library(qgraph) -> TermDocumentMatrix
tdm1 <- TermDocumentMatrix(corpus1,
                           control = list(removePunctuation = T, removeNumbers = T, wordLengths = c(2, Inf)))
tdm1_mat <- as.matrix(tdm1)
doc_count <- rowSums(tdm1_mat)
doc_order <- order(doc_count, decreasing = T)
freq_doc <- tdm1_mat[doc_order[1 : 30], ]

co_matrix <- freq_doc %*% t(freq_doc)

par(family = "Apple SD Gothic Neo")
qgraph(co_matrix, labels = rownames(co_matrix), diag = F, layout = "spring", threshold = 3,
       vsize = log(diag(co_matrix) * 2))


# 두산 백과 TF / TF-IDF
stopwords <- read.csv("Stopword.csv", stringsAsFactors = F)
dic <- read.csv("Dictionary_Animal.csv", stringsAsFactors = F)
dic_parsing <- r_parser_r(dic$content, useEn = T, language = "ko")
dic_parsing <- gsub(" ", "  ", dic_parsing)
dic_corpus <- VCorpus(VectorSource(dic_parsing))
dic_corpus <- tm_map(dic_corpus, removeWords, stopwords$stopword)
dic_corpus <- tm_map(dic_corpus, content_transformer(tolower))
dic_dtm <- DocumentTermMatrix(dic_corpus, control = list(removePunctuation = T, removeNumbers = T,
                                                         wordLengths = c(2, Inf)))
dic_dtm <- removeSparseTerms(dic_dtm, sparse = as.numeric(0.9))
findFreqTerms(dic_dtm, lowfreq = 15)
inspect(dic_dtm)


dic_dtm_tfidf <- DocumentTermMatrix(dic_corpus, control = list(removePunctuation = T, removeNumbers = T,
                                                               wordLengths = c(2, Inf),
                                                               weighting = function(x) weightTfIdf(x, normalize = T) ))
dic_dtm_tfidf <- removeSparseTerms(dic_dtm_tfidf, sparse = as.numeric(0.9))
inspect(dic_dtm_tfidf)
findAssocs(dic_dtm_tfidf, "날개", 0.2)


dic1 <- read.csv("Dictionary_Sport.csv", stringsAsFactors = F)
dic1_parsing <- r_parser_r(dic$content, useEn = T, language = "ko")
dic1_parsing <- gsub(" ", "  ", dic1_parsing)
dic1_corpus <- VCorpus(VectorSource(dic1_parsing))
dic1_corpus <- tm_map(dic1_corpus, removeWords, stopwords$stopword)
dic1_corpus <- tm_map(dic1_corpus, content_transformer(tolower))
dic1_dtm <- DocumentTermMatrix(dic1_corpus, control = list(removeNumbers = T, removePunctuation = T,
                                                           wordLengths = c(2, Inf), 
                                                           weighting = function(x) weightTfIdf(x, normalize = T) ))
dic1_dtm <- removeSparseTerms(dic1_dtm, sparse = as.numeric(0.9))
inspect(dic1_dtm)

# 이런식으로 따로따로 DTM을 구해서는 안된다. 처음 파싱하기 전에 문서의 비율을 비슷하게 맞춘뒤
# 데이터를 합치고 파싱을 시작해야한다.

# Document Term Matrix를 데이터프레임으로 변환 -> 모델에 넣기 위해서
df_tfidf <- as.data.frame(as.matrix(dic_dtm_tfidf))
df1_tfidf <- as.data.frame(as.matrix(dic1_dtm))

randomForest()
