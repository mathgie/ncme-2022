# Load the stm package
library(stm)
# Load the excel library for reading excel files
library(readxl)

# Load math set from excel into a data frame
math_set <- read_excel("/Users/joelparrish/Documents/me/maggie/mbme/maggie_data_v4_drops.xlsx")

# Load the valid essay set from excel into a data frame
valid_set <- read_excel("/Users/joelparrish/Documents/me/maggie/mbme/valid_set.xlsx")

# Remove unwanted columns
math_data_clean <- subset(math_set, select = -c(questionStem,a,b,c,d,e,id) )

# Process the complete math questions and pass the original cleaned data frame as its metadata
math_items_processed <- textProcessor(math_data_clean$completeq, metadata = math_data_clean)

# Use stm prepDocuments function to prepare the processed items for usage with stm
math_out <- prepDocuments(
  math_items_processed$documents, 
  math_items_processed$vocab, 
  math_items_processed$meta, 
  lower.thresh = 15
)

# Run stm function on the prepared data with an expected 8 topics
system.time(STM_math_result <- stm(math_out$documents, math_out$vocab, K = 8, data = math_out$meta))

# Save the stm result to Rdata file
save(STM_math_result, file = "/Users/joelparrish/Documents/me/maggie/mbme/STM_math_result.Rdata")


# Remove unwanted columns
essay_clean <- subset(valid_set, select = -c(essay_id) )

# Process the essays and pass the original cleaned data frame as its metadata
essay_processed <- textProcessor(essay_clean$essay, metadata = essay_clean)

# Use stm prepDocuments function to prepare the processed items for usage with stm
essay_out <- prepDocuments(
  essay_processed$documents, 
  essay_processed$vocab, 
  essay_processed$meta, 
  lower.thresh = 15
)

# Run stm function on the prepared data with an expected 14 topics
system.time(STM_essay_result <- stm(essay_out$documents, essay_out$vocab, K = 14, data = essay_out$meta))

# Save the stm result to Rdata file
save(STM_essay_result, file = "/Users/joelparrish/Documents/me/maggie/mbme/STM_essay_result.Rdata")
