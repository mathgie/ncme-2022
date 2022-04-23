####seededlda
library(seededlda)
library(quanteda)
library(tidytext)


##loading in data 
math_items <- read_excel("C:/Users/Maggie/Downloads/maggie_data_v4_drops.xlsx")
View(math_items)


essays <- read_excel("C:/Users/Maggie/Downloads/valid_set (1).xlsx")
View(essays)






# Use of this package requires a dataframe of tokens
math_token_df <- math_items %>%
  tidytext::unnest_tokens(word, completeq)
math_dfm <- quanteda::dfm(math_token_df$word)

system.time(
  math_seededlda <- textmodel_lda(math_dfm, k = 8))
object.size(math_seededlda)
save(math_seededlda, file = "math_seededlda.Rdata")

essay_token_df <- essays %>%
  tidytext::unnest_tokens(word, essay)
essay_dfm <- quanteda::dfm(essay_token_df$word)

system.time(
  essay_seededlda <- textmodel_lda(essay_dfm, k = 14))
object.size(essay_seededlda)
save(essay_seededlda, file = "essay_seededlda.Rdata")
