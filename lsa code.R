##packages
library(readxl)
library(xlsx)
library(qdap)
library(dplyr)
library(lsa)

##loading in data 
maggie_data_v4_drops <- read_excel("C:/Users/Maggie/Downloads/maggie_data_v4_drops.xlsx")
View(maggie_data_v4_drops)


valid_set_1_ <- read_excel("C:/Users/Maggie/Downloads/valid_set (1).xlsx")
View(valid_set_1_)


Test_Items <- maggie_data_v4_drops

View(Test_Items)

items <- Test_Items
items

nrow(items)

qstems <- items$completeq

head(qstems)

qstems2<-valid_set_1_$essay

stems_source <- VectorSource(qstems)
stems_source <- VectorSource(qstems2)
stems_corpus <- VCorpus(stems_source)
stems_corpus

fullcorpus_dtm <-DocumentTermMatrix(stems_corpus)
fullcorpus_dtm

full_m <- as.matrix(fullcorpus_dtm)


full_frequency <- rowSums(full_m)
full_frequency

clean_corpus <- function(stems_corpus){
  corpus <- tm_map(stems_corpus, stripWhitespace)
  corpus2 <- tm_map(corpus, removePunctuation)
  corpus3 <- tm_map(corpus2, content_transformer(tolower))
  corpus4 <- tm_map(corpus3, removeWords, c(stopwords("en")))
  return(corpus4)
}

clean_corp <- clean_corpus(stems_corpus)

stems_dtm <- DocumentTermMatrix(clean_corp)
stems_dtm
head(stems_dtm)

stems_m <- as.matrix(stems_dtm)



####lsa code


##convert the Document Term Matrix to the 'text matrix' format

textmatrix <- as.textmatrix(stems_m)

lsa1 <- lsa(textmatrix, 8)
lsa1
