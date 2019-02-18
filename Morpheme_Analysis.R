install.packages("KoNLP", dependencies = T)
install.packages("digest")
install.packages("rlang")
install.packages("stringi")

library(KoNLP)
library(rlang)
library(digest)
library(stringi)
library(tm)

# KoNLP Library : https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md

useSejongDic() # 형태소 분석 하기 위해서는 reference로 삼을 사전이 필요하다.
               # 시스템(system) / 세종(sejong) 사전이 있다.
# 예시 1
sentence <- "아버지가 방에 스르륵 들어가신다."

# extractNoun() : 명사 추출
extractNoun(sentence)

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