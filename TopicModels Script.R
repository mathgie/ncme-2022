##topicmodels dependent packages code


library(readxl)
library(xlsx)
library(qdap)
library(dplyr)
library(tm)
library(topicmodels)




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






#####TopicModels Package#####
##This is what we used for the original analyses###
##Most of this original code comes from chapter 6 of the Silge and Robinson book
##Link to CRAN entry https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf 
##Link to CRAN Vignette Entry https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf 


##Basic code just to specify the model for math items, change k for the number of topics

system.time(stems_lda <- LDA(stems_dtm, k = 8, control = list(seed = 1234)))
stems_lda



##Basic code just to specify the model for essays, change k for the number of topics

system.time(stems_lda2 <- LDA(estems_dtm, k = 14, control = list(seed = 1234)))
stems_lda2



##The rest of this code was only run with the math items


##Gives the probability of each wordr appearing in each topic for LDA 
stems_topics <- tidy(stems_lda, matrix = "beta")
stems_topics






##Makes the most common 10 terms and gives a plot 
stems_top_terms <- stems_topics %>% 
  group_by(topic) %>%
  top_n(15, beta) %>% 
  ungroup () %>%
  arrange(topic, -beta)

stems_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + 
  coord_flip()





##Code for findings the greatest differences between two similar-seeming topics
##This takes awhile to run, but usually works

beta_spread <- stems_topics %>% 
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))

beta_spread



##Code for Document-Topic Probabilities

stems_docfreq <- tidy(stems_lda, matrix = "gamma")
stems_docfreq
View(stems_docfreq)

stems_topics2 <- stems_docfreq %>%
  arrange(desc(gamma))



##Code for Assignment of each word to each topic


assignments <- augment(stems_lda, data = stems_dtm)

assignments


##This package also allows for CTM. Here is the code
stems_ctm <- CTM(stems_dtm, 4, method = "VEM", control=list(seed=831))

stems_ctm



##Gives the probability of each word appearing in each topic for CTM
ctm_topicfreq <- tidy(stems_ctm, matrix = "beta")
ctm_topicfreq


ctm_topics <- ctm_topicfreq %>%
  arrange(desc(beta))

ctm_topics



##Construct the adjacency matrix for a topic graph after running a CTM
##lambda has to be between 0 and 1 it seems but does not work well with 0

ctm_matrix1 <- build_graph(stems_ctm, 0.5, and = TRUE)
ctm_matrix1