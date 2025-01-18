library(stringr)
library(tm)


text <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)


text$Reviewer_Name <- tolower(text$Reviewer_Name) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Review_Date <- tolower(text$Review_Date) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Processed_Review <- tolower(text$Processed_Review) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()


text

write.csv(text, "D:/ds f/Cleaned_Emojis_Emoticons.csv", row.names = FALSE)

#------------------------------------------------------------------------------------------------------------
# Load required libraries
library(stringr)
library(tm)
library(tokenizers)

# Load the data
text <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

# Clean specific columns
text$Reviewer_Name <- tolower(text$Reviewer_Name) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Review_Date <- tolower(text$Review_Date) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Processed_Review <- tolower(text$Processed_Review) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

# Tokenize specific columns
tokens_reviewer_name <- unlist(tokenize_words(text$Reviewer_Name))
tokens_review_date <- unlist(tokenize_words(text$Review_Date))
tokens_processed_review <- unlist(tokenize_words(text$Processed_Review))

# View tokens
tokens_reviewer_name
tokens_review_date
tokens_processed_review

#------------------------------------------------------------------------------------------------------------
# Load required libraries
library(stringr)
library(tm)
library(tokenizers)
library(stopwords)

# Load the data
text <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

# Clean specific columns
text$Reviewer_Name <- tolower(text$Reviewer_Name) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Review_Date <- tolower(text$Review_Date) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Processed_Review <- tolower(text$Processed_Review) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

# Tokenize specific columns
tokens_reviewer_name <- unlist(tokenize_words(text$Reviewer_Name))
tokens_review_date <- unlist(tokenize_words(text$Review_Date))
tokens_processed_review <- unlist(tokenize_words(text$Processed_Review))

# Stop words removal
tokens_reviewer_name <- tokens_reviewer_name[!tokens_reviewer_name %in% stopwords("en")]
tokens_review_date <- tokens_review_date[!tokens_review_date %in% stopwords("en")]
tokens_processed_review <- tokens_processed_review[!tokens_processed_review %in% stopwords("en")]

# View tokens after stop words removal
tokens_reviewer_name
tokens_review_date
tokens_processed_review


#------------------------------------------------------------------------------------------------------------
# Load required libraries
library(stringr)
library(tm)
library(tokenizers)
library(stopwords)
library(SnowballC)

# Load the data
text <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

# Clean specific columns
text$Reviewer_Name <- tolower(text$Reviewer_Name) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Review_Date <- tolower(text$Review_Date) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Processed_Review <- tolower(text$Processed_Review) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

# Tokenize specific columns
tokens_reviewer_name <- unlist(tokenize_words(text$Reviewer_Name))
tokens_review_date <- unlist(tokenize_words(text$Review_Date))
tokens_processed_review <- unlist(tokenize_words(text$Processed_Review))

# Stop words removal
tokens_reviewer_name <- tokens_reviewer_name[!tokens_reviewer_name %in% stopwords("en")]
tokens_review_date <- tokens_review_date[!tokens_review_date %in% stopwords("en")]
tokens_processed_review <- tokens_processed_review[!tokens_processed_review %in% stopwords("en")]

# Apply stemming
stemmed_tokens_reviewer_name <- wordStem(tokens_reviewer_name, language = "en")
stemmed_tokens_review_date <- wordStem(tokens_review_date, language = "en")
stemmed_tokens_processed_review <- wordStem(tokens_processed_review, language = "en")

# View stemmed tokens
stemmed_tokens_reviewer_name
stemmed_tokens_review_date
stemmed_tokens_processed_review


#------------------------------------------------------------------------------------------------------------



# Load required libraries
library(stringr)
library(tm)
library(tokenizers)
library(stopwords)
library(SnowballC)
library(textstem)

# Load the data
text <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

# Clean specific columns
text$Reviewer_Name <- tolower(text$Reviewer_Name) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Review_Date <- tolower(text$Review_Date) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Processed_Review <- tolower(text$Processed_Review) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

# Tokenize specific columns
tokens_reviewer_name <- unlist(tokenize_words(text$Reviewer_Name))
tokens_review_date <- unlist(tokenize_words(text$Review_Date))
tokens_processed_review <- unlist(tokenize_words(text$Processed_Review))

# Stop words removal
tokens_reviewer_name <- tokens_reviewer_name[!tokens_reviewer_name %in% stopwords("en")]
tokens_review_date <- tokens_review_date[!tokens_review_date %in% stopwords("en")]
tokens_processed_review <- tokens_processed_review[!tokens_processed_review %in% stopwords("en")]

# Apply Lemmatization
lemmatized_tokens_reviewer_name <- lemmatize_words(tokens_reviewer_name)
lemmatized_tokens_review_date <- lemmatize_words(tokens_review_date)
lemmatized_tokens_processed_review <- lemmatize_words(tokens_processed_review)

# View lemmatized tokens
lemmatized_tokens_reviewer_name
lemmatized_tokens_review_date
lemmatized_tokens_processed_review

#------------------------------------------------------------------------------------------------------------


# Load required libraries
library(stringr)
library(tm)
library(tokenizers)
library(stopwords)
library(SnowballC)
library(textstem)
library(textclean)

# Load the data
text <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

# Clean specific columns
text$Reviewer_Name <- tolower(text$Reviewer_Name) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Review_Date <- tolower(text$Review_Date) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Processed_Review <- tolower(text$Processed_Review) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

# Handle contractions in specific columns
expanded_reviewer_name <- replace_contraction(text$Reviewer_Name)
expanded_review_date <- replace_contraction(text$Review_Date)
expanded_processed_review <- replace_contraction(text$Processed_Review)

# View the expanded text
expanded_reviewer_name
expanded_review_date
expanded_processed_review


#------------------------------------------------------------------------------------------------------------

# Load required libraries
library(stringr)
library(tm)
library(tokenizers)
library(stopwords)
library(SnowballC)
library(textstem)
library(textclean)

# Load the data
text <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

# Clean specific columns
text$Reviewer_Name <- tolower(text$Reviewer_Name) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Review_Date <- tolower(text$Review_Date) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Processed_Review <- tolower(text$Processed_Review) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

# Handle emojis in specific columns by replacing them with <emoji>
emoji_reviewer_name <- replace_emoji(text$Reviewer_Name, replacement = " <emoji> ")
emoji_review_date <- replace_emoji(text$Review_Date, replacement = " <emoji> ")
emoji_processed_review <- replace_emoji(text$Processed_Review, replacement = " <emoji> ")

# View the text with emojis replaced
emoji_reviewer_name
emoji_review_date
emoji_processed_review
#------------------------------------------------------------------------------------------------------------

# Load required libraries
library(stringr)
library(tm)
library(tokenizers)
library(stopwords)
library(SnowballC)
library(textstem)
library(textclean)

# Load the data
text <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

# Clean specific columns
text$Reviewer_Name <- tolower(text$Reviewer_Name) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Review_Date <- tolower(text$Review_Date) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

text$Processed_Review <- tolower(text$Processed_Review) %>%
  str_replace_all("[^[:alnum:][:space:]!,.]", " ") %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

# Handle contractions
expanded_reviewer_name <- replace_contraction(text$Reviewer_Name)
expanded_review_date <- replace_contraction(text$Review_Date)
expanded_processed_review <- replace_contraction(text$Processed_Review)

# Correct internet slang and misspelled words in each column
corrected_reviewer_name <- replace_internet_slang(expanded_reviewer_name)
corrected_review_date <- replace_internet_slang(expanded_review_date)
corrected_processed_review <- replace_internet_slang(expanded_processed_review)

corrected_reviewer_name <- replace_misspelling(corrected_reviewer_name)
corrected_review_date <- replace_misspelling(corrected_review_date)
corrected_processed_review <- replace_misspelling(corrected_processed_review)

# Combine the corrected columns into a new dataframe
corrected_text <- data.frame(
  Reviewer_Name = corrected_reviewer_name,
  Review_Date = corrected_review_date,
  Processed_Review = corrected_processed_review
)

# View the corrected text
corrected_text


# Assuming the corrected_text dataframe is already created as per the previous steps

# Save the corrected data to a new CSV file
write.csv(corrected_text, "D:/ds f/processed_data1.csv", row.names = FALSE)

# Confirm the save
cat("Data has been saved to 'D:/ds f/processed_data1.csv'")


