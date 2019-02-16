install.packages("KoNLP", dependencies = T)
install.packages("digest")
install.packages("rlang")
install.packages("stringi")

library(KoNLP)
library(rlang)
library(digest)
library(stringi)

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
