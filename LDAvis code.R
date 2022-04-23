##packages
library(readxl)
library(xlsx)
library(qdap)
library(dplyr)
library(tm)
library(topicmodels)
library(LDAvis)


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




##LDAVis needs a topic model from the topicmodels package 

##code for LDA for math items
system.time(items_lda <- LDA(stems_dtm, k = 8, control = list(seed = 1234)))
items_lda


system.time(essays_lda <- LDA(estems_dtm, k = 14, control = list(seed = 1234)))
            essays_lda

####LDAvis code for the math items corpus

#Convert the output of a topicmodels Latent Dirichlet Allocation to JSON for use with LDAvis
##Need to use the LDA package or some other LDA creation **FIRST** then use this
#List of parameters:
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export

topicmodels_json_ldavis <- function(items_lda, stems_dtm){
  require(LDAvis)
  require(slam)
  
  # Find required quantities
  phi <- as.matrix(topicmodels::posterior(items_lda)$terms)
  theta <- as.matrix(topicmodels::posterior(items_lda)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(stems_dtm)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = as.vector(table(stems_dtm$i)),
                                 term.frequency = term_freq)
  
  return(json_lda)
}


system.time(json_res <- topicmodels_json_ldavis(items_lda, stems_dtm))

serVis(json_res)



##LDAvis code for the essay corpus


topicmodels_json_ldavis <- function(essays_lda, estems_dtm){
  require(LDAvis)
  require(slam)
  
  # Find required quantities
  phi <- as.matrix(topicmodels::posterior(essays_lda)$terms)
  theta <- as.matrix(topicmodels::posterior(essays_lda)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(estems_dtm)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = as.vector(table(estems_dtm$i)),
                                 term.frequency = term_freq)
  
  return(json_lda)
}



system.time(json_res <- topicmodels_json_ldavis(essays_lda, estems_dtm))

serVis(json_res)
