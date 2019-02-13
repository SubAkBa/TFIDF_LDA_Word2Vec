library(httr)
library(dplyr)
library(rvest)
library(RSelenium)
library(gdata)
library(stringr)

rm(list = ls()); gc(reset = T)

# 네이버 뉴스 : 디지털 데일리
main_url <- paste0("https://news.naver.com/main/read.nhn?mode=LPOD&mid=sec&oid=138&aid=000")
article_num <- 2070040
article_num <- 2058527

digit_daily_articles <- data.frame()
digit_daily_articles <- read.csv("Digital_Daily.csv", stringsAsFactors = F)

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

# 2058528 - 2035653 = 22875
write.csv(digit_daily_articles, "Digital_Daily.csv", row.names = F)



# 네이버 지식백과 : 두산백과
main_url <- paste0("https://terms.naver.com/list.nhn?cid=40942&categoryId=40942")
part_url <- "https://terms.naver.com"
page_url <- "&page="
# subject_idx <- 1; page_idx <- 1; dict_idx <- 1
# subject_idx <- 2; page_idx <- 468; dict_idx <- 2
subject_idx <- 6; page_idx <- 301; dict_idx <- 11

dictionary <- data.frame()
subject_part <- read_html(main_url) %>% html_nodes(".subject_item a")
subject_list <- subject_part %>% html_attr("href")
subject_name <- subject_part %>% html_text()

for(subject_idx in 1 : length(subject_list)){
  part_page_url <- paste0(part_url, subject_list[subject_idx], page_url)
  page_idx <- 1
  
  while((prev_page <- read_html(paste0(part_page_url, page_idx)) %>% 
         html_nodes(".content_list .subject a:first-child") %>% html_attr("href"))[1] 
        != 
        (next_page <- read_html(paste0(part_page_url, page_idx + 1)) %>% 
         html_nodes(".content_list .subject a:first-child") %>% html_attr("href"))[1]){
    dict_list <- prev_page
    
    for(dict_idx in 1 : length(dict_list)){
      dict_main <- read_html(paste0(part_url, dict_list[dict_idx])) %>% html_node("#content")
      
      title <- dict_main %>% html_node(".headword_title h2") %>% html_text()
      sub_name <- subject_name[subject_idx]
      content_bag <- gsub("[\n|\t]", "", dict_main %>% html_nodes("#size_ct .txt") %>% html_text() %>% trimws())
      content <- c()
      for(content_idx in 1 : length(content_bag))
        content <- paste(content, content_bag[content_idx]) %>% trim()
        
      dict_temp <- data.frame(title, sub_name, content)
      dictionary <- rbind(dictionary, dict_temp)
      
      print(paste0("subject : ", subject_idx, " / page_idx : ", page_idx, " / dict_idx : ", dict_idx))
    }
    
    page_idx <- page_idx + 1
  }
}
# subject = 2 / page_idx = 468 / dict_idx = 1
dictionary_philosophy <- dictionary %>% filter(sub_name == "철학")
write.csv(dictionary_philosophy, "Dictionary_Philosophy.csv", row.names = F)
dictionary <- setdiff(dictionary, dictionary_philosophy)
write.csv(dictionary, "Dictionary_Religion.csv", row.names = F)

# subject = 2 / page_idx = 623 / dict_idx = 15
dictionary_religion <- read.csv("Dictionary_Religion.csv", stringsAsFactors = F)
dictionary_religion <- rbind(dictionary_religion, dictionary)
write.csv(dictionary_religion, "Dictionary_Religion.csv", row.names = F)

# subject = 6 / page_idx = 301 / dict_idx = 10
dictionary_group <- split(dictionary, dictionary$sub_name)
dictionary_socialscience <- dictionary_group$사회과학
write.csv(dictionary_socialscience, "Dictionary_SocialScience.csv", row.names = F)
dictionary_ecobusi <- dictionary_group$`경제·경영`
write.csv(dictionary_ecobusi, "Dictionary_EconomicsBusiness.csv", row.names = F)
dictionary_travel <- dictionary_group$여행
write.csv(dictionary_travel, "Dictionary_Travel.csv", row.names = F)
dictionary_sport <- dictionary_group$스포츠
write.csv(dictionary_sport, "Dictionary_Sport.csv", row.names = F)

# Rbind sport data(Dictionary_Sport 끝부분 2개가 중복)
dictionary_sport <- read.csv("Dictionary_Sport.csv", stringsAsFactors = F)
dictionary_sport <- dictionary_sport[1 : 4509, ]
dictionary_sport <- rbind(dictionary_sport, dictionary)
write.csv(dictionary_sport, "Dictionary_Sport.csv", row.names = F)

# subject = 7 / page_idx = 999 / dict_idx = 15
dictionary_life <- dictionary
write.csv(dictionary_life, "Dictionary_Life.csv", row.names = F)
tail(dictionary_life, 2) # 샤코티스 / 상마연