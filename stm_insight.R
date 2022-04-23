suppressPackageStartupMessages(library(stm))

library(readxl)

maggie_data_v4_drops <- read_excel("/Users/joelparrish/Documents/me/maggie/mbme/maggie_data_v4_drops.xlsx")
valid_set <- read_excel("/Users/joelparrish/Documents/me/maggie/mbme/valid_set.xlsx")

maggie_data_clean <- subset(maggie_data_v4_drops, select = -c(questionStem,a,b,c,d,e,id) )

math_items_processed <- textProcessor(maggie_data_clean$completeq, metadata = maggie_data_clean)
out <- prepDocuments(math_items_processed$documents, math_items_processed$vocab, math_items_processed$meta, lower.thresh = 15)
math_items_docs <- out$documents
math_items_vocab <- out$vocab
math_items_meta <- out$meta

system.time(STM_math_result <- stm(math_items_docs, math_items_vocab, K = 8, data = math_items_meta))

object.size(STM_math_result) 
save(STM_math_result, file = "/Users/joelparrish/Documents/me/maggie/mbme/STM_math_result.Rdata")


essay_clean <- subset(valid_set, select = -c(essay_id) )

essay_processed <- textProcessor(essay_clean$essay, metadata = essay_clean)
out <- prepDocuments(essay_processed$documents, essay_processed$vocab, essay_processed$meta, lower.thresh = 15)
essay_docs <- out$documents
essay_vocab <- out$vocab
essay_meta <- out$meta

system.time(STM_essay_result <- stm(essay_docs, essay_vocab, K = 14, data = essay_meta))

object.size(STM_essay_result) 
save(STM_essay_result, file = "/Users/joelparrish/Documents/me/maggie/mbme/STM_essay_result.Rdata")
