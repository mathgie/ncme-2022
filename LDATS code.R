##packages
library(readxl)
library(xlsx)
library(qdap)
library(dplyr)
library(tm)
library(LDATS)

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


##Preprocessing for essay corpus

View(valid_set_1_)


essay_Items <- valid_set_1_

View(essay_Items)


estems <- essay_Items$essay

head(estems)


estems_source <- VectorSource(estems)
estems_corpus <- VCorpus(estems_source)
estems_corpus

efullcorpus_dtm <-DocumentTermMatrix(estems_corpus)
efullcorpus_dtm

full_m <- as.matrix(efullcorpus_dtm)


efull_frequency <- rowSums(full_m)
efull_frequency

clean_corpus <- function(estems_corpus){
  corpus <- tm_map(estems_corpus, stripWhitespace)
  corpus2 <- tm_map(corpus, removePunctuation)
  corpus3 <- tm_map(corpus2, content_transformer(tolower))
  corpus4 <- tm_map(corpus3, removeWords, c(stopwords("en")))
  return(corpus4)
}

clean_corp <- clean_corpus(estems_corpus)

estems_dtm <- DocumentTermMatrix(clean_corp)
estems_dtm
head(estems_dtm)


estems_m <- as.matrix(estems_dtm)


####LDATS for math items

system.time(lda_model_set2 <- LDA_set(document_term_table = stems_m,
                          topics = c(2:8),
                          nseeds = 2))


selected_lda_model <- select_LDA(lda_model_set2)

selected_lda_model[[1]]@k

head(selected_lda_model[[1]]@gamma)

plot(selected_lda_model[[1]])



####LDATS for essaycorpus

system.time(lda_model_set2 <- LDA_set(document_term_table = estems_m,
                                      topics = c(2:14),
                                      nseeds = 2))


selected_lda_model <- select_LDA(lda_model_set2)

selected_lda_model[[1]]@k

head(selected_lda_model[[1]]@gamma)

plot(selected_lda_model[[1]])