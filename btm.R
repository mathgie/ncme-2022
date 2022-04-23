# Load the BTM package
library(BTM)

# Tokenizes data frame with one row per token with an id column
library(tokenizers)

# Reads excel data into a data frame
library(readxl)

# List of stop words
library(stopwords)

# Load math set from excel into a data frame
math_set <- read_excel("/Users/joelparrish/Documents/me/maggie/mbme/maggie_data_v4_drops.xlsx")

# Load the valid essay set from excel into a data frame
valid_set <- read_excel("/Users/joelparrish/Documents/me/maggie/mbme/valid_set.xlsx")

# Function to clean up the math set
clean_data_math <- function(source_items) {
  cleaned_items <- c()
  
  for(i in 1:nrow(source_items)){
    text_vec <- unlist(tokenize_words(source_items$completeq[i]))
    text_vec <- tm::removeWords(text_vec, stopwords("english"))
    text_vec <- text_vec[text_vec != ""]
    
    rdf <- data.frame(
      id = source_items$id[i],
      word_token = text_vec
    )
    
    cleaned_items <- rbind(cleaned_items, rdf)
  }
  
  return(cleaned_items)
}

# Clean the set and store it into a variable
math_items_BTM <- clean_data_math(math_set)
# Save the math items in Rdata file
save(math_items_BTM, file = "math_items_BTM.Rdata")


# Function to clean the essay data
clean_data_eassy <- function(source_items) {
  cleaned_items <- c()
  
  for(i in 1:nrow(source_items)){
    text_vec <- unlist(tokenize_words(source_items$essay[i]))
    text_vec <- tm::removeWords(text_vec, stopwords("english"))
    text_vec <- text_vec[text_vec != ""]
    
    rdf <- data.frame(
      id = source_items$essay_id[i],
      word_token = text_vec
    )
    
    cleaned_items <- rbind(cleaned_items, rdf)
  }
  
  return(cleaned_items)
}

# Clean the set and store it into a variable
essay_BTM <- clean_data_eassy(valid_set)
# Save the essay items in Rdata file
save(essay_BTM, file = "essay_BTM.Rdata")


# Run the BTM function on the math data set with an expected 8 topics
system.time(BTM_math_result <- BTM(math_items_BTM, k = 8))

# Save the results to a Rdata file
save(BTM_math_result, file = "/Users/joelparrish/Documents/me/maggie/mbme/BTM_math_result.Rdata")


# Run the BTM function on the essay data set with an expected 14 topics
system.time(BTM_essay_result <- BTM(essay_BTM, k = 14))

# Save the results to a Rdata file
save(BTM_essay_result, file = "/Users/joelparrish/Documents/me/maggie/mbme/BTM_essay_result.Rdata")
