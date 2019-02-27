install.packages("tidytext")
install.packages("topicmodels")

library(ggplot2)
library(dplyr)
library(tidyr)
library(NLP4kec)
library(tm)
library(tidytext)
library(topicmodels)

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

