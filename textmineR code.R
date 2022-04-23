##textmineR dependent packages


library(readxl)
library(xlsx)
library(qdap)
library(dplyr)
library(tm)
library(textmineR)




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

eitems <- essay_Items


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




##Example for Math Items


##making the dtm

dtm <- CreateDtm(doc_vec = items$completeq, # character vector of documents
                 doc_names = items$id, # document names, optional
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # by default, this will be the max number of cpus available


tf_mat <- TermDocFreq(dtm = dtm)

# Fit a Latent Dirichlet Allocation model
# note the number of topics is arbitrary here
# see extensions for more info

set.seed(12345)

system.time(model <- FitLdaModel(dtm = dtm, 
                     k = 8,
                     iterations = 200, # I usually recommend at least 500 iterations or more
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) )

str(model)

model$r2

plot(model$log_likelihood, type = "l")

summary(model$coherence)

hist(model$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

model$top_terms <- GetTopTerms(phi = model$phi, M = 5)
head(model$top_terms)




###Example for Essay Corpus


##making the dtm

dtm2 <- CreateDtm(doc_vec = eitems$essay, # character vector of documents
                 doc_names = eitems$essay_id, # document names, optional
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # by default, this will be the max number of cpus available


tf_mat <- TermDocFreq(dtm = dtm2)

# Fit a Latent Dirichlet Allocation model
# note the number of topics is arbitrary here
# see extensions for more info

set.seed(12345)

system.time(model2 <- FitLdaModel(dtm = dtm2, 
                     k = 14,
                     iterations = 200, # I usually recommend at least 500 iterations or more
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) )

str(model2)

model2$r2

plot(model2$log_likelihood, type = "l")

summary(model2$coherence)

hist(model2$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

model2$top_terms <- GetTopTerms(phi = model$phi, M = 5)
head(model2$top_terms)