install.packages("textTinyR")

library(textTinyR)
library(dplyr)

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

# word2vec text 파일
write.table(doosan_data$content, file = "Dictionary_TSL.txt", row.names = F, col.names = F)




model <- word2vec("Dictionary_TSL.txt", output_file = "Dictionary_TSL.bin", threads = 3, vectors = 100, window = 5)
read.vectors("Digital_Daily.bin")