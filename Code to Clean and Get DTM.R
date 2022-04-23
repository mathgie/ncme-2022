options(java.parameters = "- Xmx2048m")

library(qdap)
library(dplyr)
library(tm)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(readxl)
library(xlsx)
library(topicmodels)
library(textdata)
library(tidytext)
library(tidyr)
library(textmineR)
library(quanteda)
library(ldatuning)



##Change in the file between mine and the Kaggle set
maggie_data_v4_drops_tsv <- read_excel("C:/Users/Maggie/Downloads/maggie_data_v4_drops.tsv.xlsx")
View(maggie_data_v4_drops_tsv)

valid_set <- read_excel("C:/Users/Maggie/Downloads/valid_set.xlsx")
View(valid_set)



####Start Bag of Words Code

##Loading in data and exploring how it looks
Test_Items <- valid_set

View(Test_Items)

items <- Test_Items
items

nrow(items)

completeq <- items$essay

stems_source <- VectorSource(completeq)

stems_corpus <- VCorpus(stems_source)

stems_corpus

##Initial DTM before cleaning for stop words
fullcorpus_dtm <-DocumentTermMatrix(stems_corpus)
fullcorpus_dtm


##Calculation for initial overall word count before cleaning = word count per question
##includes stop words
full_m <- as.matrix(fullcorpus_dtm)

full_frequency <- rowSums(full_m)
full_frequency

typeof(full_frequency)
View(full_frequency)


##Function to clean the corpus
##add in custom stop words here, as needed
##corpus 5 deletes numbers, can remove as needed
clean_corpus <- function(stems_corpus){
  corpus <- tm_map(stems_corpus, stripWhitespace)
  corpus2 <- tm_map(corpus, removePunctuation)
  corpus3 <- tm_map(corpus2, content_transformer(tolower))
  corpus4 <- tm_map(corpus3, removeWords, c(stopwords("en")))
  return(corpus4)
}


##Cleaning the corpus with the function and checking to make sure it was cleaned
clean_corp <- clean_corpus(stems_corpus)
clean_corp[[20]][1]
clean_corp

##Making a DTM with the cleaned data
complete_dtm <- DocumentTermMatrix(clean_corp)
complete_dtm
head(complete_dtm)
stems_m <- as.matrix(complete_dtm)
dim(stems_m)

##Checking a portion of the DTM
stems_m[14:16, 100:105]



##Code to Make a TDM (if needed)
stems_tdm <-TermDocumentMatrix(clean_corp)
stems_tdm
stems2_m <- as.matrix(stems_tdm)
dim(stems2_m)
stems2_m[100:105, 14:16]


