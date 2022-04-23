library(stm)
library(stmCorrViz)
library(readxl)

# Load math set from excel into a data frame
math_set <- read_excel("C:/Users/Maggie/Downloads/maggie_data_v4_drops.xlsx")

# Load the valid essay set from excel into a data frame
valid_set <- read_excel("C:/Users/Maggie/Downloads/valid_set (1).xlsx")

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

# Specify output file of html result. The filename must have the extension .html
math_out_file <- "C:/Users/Maggie/Downloads/corrvizout.html"

# Runs the stmCorrViz function with stm variable, file, text vector, and the stm dtm
system.time(stmCorrViz(
  STM_math_result, 
  math_out_file, 
  math_data_clean$completeq, 
  out$documents, 
  title = "STM Math Results",
  search_options = list(range_min = 0.05, range_max = 5, step = 0.05)
))


# Remove unwanted columns
essay_clean <- subset(valid_set, select = -c(essay_id) )

# Process the complete math questions and pass the original cleaned data frame as its metadata
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

essay_out_file2 <- "C:/Users/Maggie/Downloads/corrvizout2.html"

system.time(stmCorrViz(
  STM_essay_result, 
  essay_out_file2, 
  essay_clean$essay, 
  essay_out$documents, 
  title = "STM Essay Results"
))
