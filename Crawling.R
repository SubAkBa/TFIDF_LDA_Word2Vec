library(httr)
library(dplyr)
library(rvest)
library(RSelenium)
library(gdata)
library(stringr)

rm(list = ls()); gc(reset = T)

main_url <- paste0("https://news.naver.com/main/read.nhn?mode=LPOD&mid=sec&oid=138&aid=000")
article_num <- 2070040

digit_daily_articles <- data.frame()

# 16년도 ~ 현재 까지 (디지털 데일리)
while(article_num >= 2035653){
  article_html <- read_html(paste0(main_url, article_num))
  
  article_body <- article_html %>% html_node("#main_content")
  title <- article_body %>% html_node("#articleTitle") %>% html_text() %>% trimws()
  date <- str_split(article_body %>% html_nodes(".sponsor") %>% html_text() %>% 
                      trimws(), "\r", simplify = T)[1] %>% substr(6, 21)
  text <- str_split(article_body %>% html_node("#articleBodyContents") %>% 
                                   html_text() %>% trimws(), "기자")[[1]][2]
  abody <- substr(text, 3, nchar(text) - 5)
  reporter <- str_sub(text, -4, -2)
  
  article_temp <- data.frame(title, reporter, date, abody)
  digit_daily_articles <- rbind(digit_daily_articles, article_temp)
  print(paste("count :", article_num - 2035653))
  
  article_num <- article_num - 1
}
View(digit_daily_articles)

#  - 2035653 = 22875
write.csv(digit_daily_articles, "Digital_Daily.csv", row.names = F)
