library(tm)            # Text Mining for text cleaning and stopwords removal
library(tokenizers)    # Tokenization
library(textclean)     # Spell checking and handling contractions
library(stringr)       # String manipulation
library(SnowballC)     # Stemming
library(textstem)      # Lemmatization
library(qdapRegex)     # Handling Emojis and Emoticons
library(hunspell)      #spell checking and correcting misspelled words

#console install packages:   install.packages(c("stringr", "tm", "tokenizers", "SnowballC", "textstem", "textclean", "hunspell","qdapRegex"))

 

#read your text fle
text <- read.table("F:/dd/file_name.txt", header = TRUE, sep = "\t", fill = TRUE) #file path and file name
text




#Text Cleaning
text1 <- tolower(text)                            # Convert to lowercase                        
text2 <- str_replace_all(text1, "[^[:alnum:][:space:]!,.]", " ") # Remove special characters except punctuation
text3 <- removePunctuation(text2)        # Remove punctuation
text4 <- removeNumbers(text3)            # Remove numbers
text5 <- stripWhitespace(text4)          # Remove extra whitespace
text5



#Tokenization
tokens <- unlist(tokenize_words(text5))
tokens

#Stop Words Removal
tokens <- tokens[!tokens %in% stopwords("en")]
tokens

#	Stemming  
stemmed_tokens <- wordStem(tokens, language = "en")
stemmed_tokens



#Lemmatization
lemmatized_tokens <- lemmatize_words(tokens)
lemmatized_tokens



#Handling Contractions
expanded_text <- replace_contraction(text5)
expanded_text



#Handling Emojis and Emoticons
emoji_text <- replace_emoji(text, replacement = " <emoji> ")
emoji_text



#Spell Checking: Correct misspelled words
corrected_text <- replace_internet_slang(expanded_text)  # Correct informal words/slangs
corrected_text <- replace_misspelling(corrected_text)   # Correct misspellings
corrected_text


#csv file making
write.csv(corrected_text, "F:/dd/processed_data.csv", row.names = FALSE)
