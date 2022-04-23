## dependent packages
library(readxl)
library(xlsx)
library(lda)
library(LDAShiny)


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



####Package LDA####
##Link to CRAN entry https://cran.r-project.org/web/packages/lda/lda.pdf  
##Link to Github where this comes from https://github.com/slycoder/R-lda


#need this function to generate a documents matrix comprised of
#line by line info and it also generates a vocab vector
#we need to pass both to get the lda to work in this package

##Lexicalize code: https://www.rdocumentation.org/packages/lda/versions/1.4.2/topics/lexicalize
##This can also take a few minutes, depending on the corpus size
corpus <- lexicalize(qstems, lower=TRUE)


start_time3 <- Sys.time()
##code for Lda for 8 clusters in math items
K <- 8 ## Num clusters
result <- lda.collapsed.gibbs.sampler(corpus$documents,
                                      K,  ## Num clusters
                                      corpus$vocab,
                                      25,  ## Num iterations
                                      0.1,
                                      0.1,
                                      compute.log.likelihood=TRUE) 

end_time3 <- Sys.time()

end_time3 - start_time3




## Get the top words in the cluster
top.words <- top.topic.words(result$topics, 5, by.score=TRUE)
View(top.words)

## Number of documents to display
N <- 10

topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions <-
  topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] <-  1 / K

colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")

topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:N)),
                             variable.name="topic",
                             id.vars = "document")  

ggplot(topic.proportions.df, aes(x=topic, y=value, fill=document), ylab="proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  coord_flip() +
  facet_wrap(~ document, ncol=5)

##LDAShiny

LDAShiny::runLDAShiny()
