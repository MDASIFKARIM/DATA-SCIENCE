library(dplyr)
library(rvest)


# Initialize an empty data frame to store results
mods <- data.frame()

# Loop through page numbers from 1 to 10
for (page_number in 1:28) {
  # Construct the URL
  link <- paste0("https://www.goodreads.com/book/show/216351768/reviews?reviewFilters=eyJhZnRlciI6Ik5UVXdMREUzTXpZMU5ERTROems1TmpBIn0%3D", page_number)
  # Read the HTML content
  page <- read_html(link)
  # Extract product details
  Reviewer_Name = page %>% html_nodes(".ReviewerProfile__name a") %>% html_text()
  Review_Date =  page %>% html_nodes(".Text__body3 a") %>% html_text()
  Review = page %>% html_nodes(".Formatted") %>% html_text()
  # Ensure vectors are of equal length
  max_length <- max(length(Reviewer_Name), length(Review_Date), length(Review))
  Reviewer_Name <- c(Reviewer_Name, rep(NA, max_length - length(Reviewer_Name)))
  Review_Date <- c(Review_Date, rep(NA, max_length - length(Review_Date)))
  Review <- c(Review, rep(NA, max_length - length(Review)))
  # Create a data frame for the current page
  product <- data.frame(Reviewer_Name, Review_Date, Review, stringsAsFactors = FALSE)
  # Combine the current page's data with the main data frame
  mods <- bind_rows(mods, product)
  # Print the current page number
  print(paste("Page:", page_number, "scraped successfully."))
}

# Write the collected data to a CSV file
write.csv(mods, "E:/Data Science/Datasets.csv", row.names = FALSE)

# View the collected data
View(mods)










library(dplyr)
library(stringr)
library(textclean)


data <- read.csv("E:/Data Science/Datasets.csv", stringsAsFactors = FALSE)


clean_text <- function(text) {
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


data <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


write.csv(data, "E:/Data Science/Cleaned_Data.csv", row.names = FALSE)


View(data)






library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)


data <- read.csv("E:/Data Science/Cleaned_Data.csv", stringsAsFactors = FALSE)


format_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  
  sentence_tokens <- tokenize_sentences(text)[[1]]
  
  
  formatted_sentences <- sapply(sentence_tokens, function(sentence) {
    word_tokens <- tokenize_words(sentence)[[1]]
    paste0("[", paste0("'", word_tokens, "'", collapse = ", "), "]")
  })
  
  
  return(paste(formatted_sentences, collapse = " "))
}


data_tokenized <- data%>%
  mutate(across(everything(), ~ sapply(., format_tokens)))


write.csv(data_tokenized, "E:/Data Science/tokenized.csv", row.names = FALSE)


View(data_tokenized)






if (!require("tokenizers")) install.packages("tokenizers")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("textclean")) install.packages("textclean")
if (!require("stopwords")) install.packages("stopwords")

library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(stopwords)


data <- read.csv("E:/Data Science/tokenized.csv", stringsAsFactors = FALSE)
data


process_tokens <- function(data) {
  if (is.na(data) || !is.character(data) || nchar(data) == 0) {
    return(NA)  
  }
  
  
  word_tokens <- tokenize_words(data)[[1]]
  
  
  stop_words <- stopwords::stopwords("en")  
  filtered_tokens <- word_tokens[!(tolower(word_tokens) %in% stop_words)]
  
  
  formatted_tokens <- paste0("[", paste0("'", filtered_tokens, "'", collapse = ", "), "]")
  
  return(formatted_tokens)
}


data_filtered <- data %>%
  mutate(across(everything(), ~ sapply(., process_tokens)))


write.csv(data_filtered, "E:/Data Science/Stop words Removed Data.csv", row.names = FALSE)


View(data_filtered)







# Load required libraries
library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(stopwords)
library(SnowballC)
library(textstem)

# Read the input CSV file
data <- read.csv("E:/Data Science/Stop words Removed Data.csv", stringsAsFactors = FALSE)

# Function to process text: tokenization, stop-word removal, stemming, lemmatization
process_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  # Tokenize the text
  tokens <- unlist(tokenizers::tokenize_words(text))
  
  # Remove stop words
  filtered_tokens <- tokens[!tokens %in% stopwords::stopwords("en")]
  
  # Apply stemming
  stemmed_tokens <- SnowballC::wordStem(filtered_tokens, language = "en")
  
  # Apply lemmatization
  lemmatized_tokens <- textstem::lemmatize_words(stemmed_tokens)
  
  # Format the tokens into a string representation
  formatted_tokens <- paste0("[", paste0("'", lemmatized_tokens, "'", collapse = ", "), "]")
  
  return(formatted_tokens)
}

# Process each column of the dataset
data_processed <- data %>%
  mutate(across(everything(), ~ sapply(., process_tokens)))

# Write the processed data to a new CSV file
write.csv(data_processed, "E:/Data Science/Stemming and Lemmatization.csv", row.names = FALSE)

# View the processed data
View(data_processed)










# Load necessary libraries
library(dplyr)
library(stringr)

# Load the dataset
data <- read.csv("E:/Data Science/Stemming and Lemmatization.csv", stringsAsFactors = FALSE)


# Function to expand contractions
expand_contractions <- function(text) {
  # Define a list of common contractions and their expanded forms
  contractions <- c(
    "ive" = "I have",
    "im" = "I am",
    "hes" = "he is",
    "shes" = "she is",
    "theyre" = "they are",
    "dont" = "do not",
    "doesnt" = "does not",
    "didnt" = "did not",
    "wasnt" = "was not",
    "werent" = "were not",
    "wouldnt" = "would not",
    "cant" = "cannot",
    "couldnt" = "could not",
    "ishnt" = "is not",
    "arent" = "are not",
    "theres" = "there is",
    "youre" = "you are",
    "shouldnt" = "should not",
    "wouldve" = "would have",
    "couldve" = "could have",
    "mustve" = "must have",
    "its" = "it is"
  )
  
  # Replace contractions in the text
  for (contraction in names(contractions)) {
    text <- str_replace_all(text, contraction, contractions[contraction])
  }
  
  return(text)
}

# Apply the function to the Review column
data <- data %>%
  mutate(Review = sapply(Review, expand_contractions))

# Save the processed dataset
write.csv(data, "E:/Data Science/Handling Conractions.csv", row.names = FALSE)

# View a sample of the processed data
View(data)
















# Load necessary libraries
library(stringr)

# Load the dataset
data <- read.csv("E:/Data Science/Handling Conractions.csv", stringsAsFactors = FALSE)

# Define a function to detect emojis and emoticons
handle_emojis_emoticons <- function(text) {
  # Define regex for emojis and common emoticons
  emoji_pattern <- "[\\U0001F600-\\U0001F64F\\U0001F300-\\U0001F5FF\\U0001F680-\\U0001F6FF\\U0001F700-\\U0001F77F]+"
  emoticon_pattern <- ":\\)|:-\\)|:\\(|:-\\(|;\\)|;-\\)|:D|:-D|XD|<3"
  
  # Detect emojis and emoticons
  emojis <- str_extract_all(text, emoji_pattern)
  emoticons <- str_extract_all(text, emoticon_pattern)
  
  # Replace emojis and emoticons with placeholders
  text <- str_replace_all(text, emoji_pattern, "<EMOJI>")
  text <- str_replace_all(text, emoticon_pattern, "<EMOTICON>")
  
  return(list(
    modified_text = text,
    emojis = emojis,
    emoticons = emoticons
  ))
}

# Apply the function to the 'Review' column
data$Processed_Review <- sapply(data$Review, function(review) {
  result <- handle_emojis_emoticons(review)
  result$modified_text
})

# Remove the original 'Review' column
data$Review <- NULL

# Save the modified dataset
write.csv(data, "E:/Data Science/Handling Emojis & Emoticons.csv", row.names = FALSE)

# Output a sample of the processed data
View(data)













# Load required libraries
library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(stopwords)
library(SnowballC)
library(textstem)
library(hunspell)

# Load the dataset
data <- read.csv("E:/Data Science/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

# Function to clean text
clean_text <- function(text) {
  if (is.na(text)) return(NA)
  text <- replace_html(text)  # Replace HTML entities
  text <- tolower(text)       # Convert to lowercase
  text <- str_remove_all(text, "[^a-zA-Z\\s]")  # Remove non-alphabetic characters
  text <- str_squish(text)    # Remove extra spaces
  return(text)
}

# Enhanced spell-check function with word removal for uncorrectable words
spell_check_text <- function(text) {
  if (is.na(text) || !is.character(text)) {
    return(text)
  }
  
  tokens <- unlist(strsplit(text, "\\s+"))  # Split text into words
  corrected_tokens <- sapply(tokens, function(word) {
    if (!hunspell_check(word)) {  # If the word is misspelled
      suggestions <- hunspell_suggest(word)
      if (length(suggestions) > 0) {
        # Filter the suggestions to pick the simplest form
        valid_suggestions <- suggestions[[1]][!grepl("[-\\s]", suggestions[[1]])]  # Remove hyphenated/compound suggestions
        if (length(valid_suggestions) > 0) {
          return(tolower(valid_suggestions[1]))  # Convert suggestion to lowercase
        }
      }
      return("")  # Remove uncorrectable word
    }
    return(tolower(word))  # Convert original word to lowercase
  })
  
  # Remove empty tokens
  corrected_tokens <- corrected_tokens[corrected_tokens != ""]
  return(paste(corrected_tokens, collapse = " "))
}

# Function to format tokens
format_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  # Tokenize into sentences
  sentence_tokens <- tokenize_sentences(text)[[1]]
  
  # Format each sentence's tokens
  formatted_sentences <- sapply(sentence_tokens, function(sentence) {
    word_tokens <- tokenize_words(sentence)[[1]]
    paste0("[", paste0("'", word_tokens, "'", collapse = ", "), "]")
  })
  
  return(paste(formatted_sentences, collapse = " "))
}

# Process the data through the complete pipeline
data_processed <- data %>%
  # Step 1: Clean the text
  mutate(across(everything(), ~ sapply(., clean_text))) %>%
  # Step 2: Apply spell checking only to Processed_Review column
  mutate(Processed_Review = sapply(Processed_Review, spell_check_text)) %>%
  # Step 3: Apply tokenization
  mutate(across(everything(), ~ sapply(., format_tokens)))

# Save the processed data
write.csv(data_processed, "E:/Data Science/Processed_Data_Complete.csv", row.names = FALSE)

# View the processed data
View(data_processed)


