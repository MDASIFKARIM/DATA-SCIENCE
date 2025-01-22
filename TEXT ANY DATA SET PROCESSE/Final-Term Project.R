library(dplyr)
library(rvest)


mods <- data.frame()

for (page_number in 1:28) {
  link <- paste0("https://www.goodreads.com/book/show/216351768/reviews?reviewFilters=eyJhZnRlciI6Ik5UVXdMREUzTXpZMU5ERTROems1TmpBIn0%3D", page_number)
  page <- read_html(link)
  Reviewer_Name = page %>% html_nodes(".ReviewerProfile__name a") %>% html_text()
  Review_Date =  page %>% html_nodes(".Text__body3 a") %>% html_text()
  Review = page %>% html_nodes(".Formatted") %>% html_text()
  max_length <- max(length(Reviewer_Name), length(Review_Date), length(Review))
  Reviewer_Name <- c(Reviewer_Name, rep(NA, max_length - length(Reviewer_Name)))
  Review_Date <- c(Review_Date, rep(NA, max_length - length(Review_Date)))
  Review <- c(Review, rep(NA, max_length - length(Review)))
  product <- data.frame(Reviewer_Name, Review_Date, Review, stringsAsFactors = FALSE)
  mods <- bind_rows(mods, product)
  print(paste("Page:", page_number, "scraped successfully."))
}

write.csv(mods, "D:/ds f/Datasets.csv", row.names = FALSE)

View(mods)











library(dplyr)
library(stringr)
library(textclean)


data <- read.csv("D:/ds f/Datasets.csv", stringsAsFactors = FALSE)

data
clean_text <- function(text) {
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


data <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


write.csv(data, "D:/ds f/Cleaned_Data.csv", row.names = FALSE)


View(data)






library(tokenizers)
library(dplyr)
library(stringr)


data <- read.csv("D:/ds f/Cleaned_Data.csv", stringsAsFactors = FALSE)


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


write.csv(data_tokenized, "D:/ds f/tokenized.csv", row.names = FALSE)


View(data_tokenized)







library(dplyr)
library(stringr)
library(stopwords)


data <- read.csv("D:/ds f/tokenized.csv", stringsAsFactors = FALSE)
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


write.csv(data_filtered, "D:/ds f/Stop words Removed Data.csv", row.names = FALSE)


View(data_filtered)








library(dplyr)
library(stringr)
library(SnowballC)
library(textstem)

data <- read.csv("D:/ds f/Stop words Removed Data.csv", stringsAsFactors = FALSE)

process_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  tokens <- unlist(tokenizers::tokenize_words(text))
  
  filtered_tokens <- tokens[!tokens %in% stopwords::stopwords("en")]
  
  stemmed_tokens <- SnowballC::wordStem(filtered_tokens, language = "en")
  
  lemmatized_tokens <- textstem::lemmatize_words(stemmed_tokens)
  
  formatted_tokens <- paste0("[", paste0("'", lemmatized_tokens, "'", collapse = ", "), "]")
  
  return(formatted_tokens)
}

data_processed <- data %>%
  mutate(across(everything(), ~ sapply(., process_tokens)))

write.csv(data_processed, "D:/ds f/Stemming and Lemmatization.csv", row.names = FALSE)

View(data_processed)










library(dplyr)
library(stringr)

data <- read.csv("D:/ds f/Stemming and Lemmatization.csv", stringsAsFactors = FALSE)


expand_contractions <- function(text) {
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
  
  for (contraction in names(contractions)) {
    text <- str_replace_all(text, contraction, contractions[contraction])
  }
  
  return(text)
}

data <- data %>%
  mutate(Review = sapply(Review, expand_contractions))

write.csv(data, "D:/ds f/Handling Conractions.csv", row.names = FALSE)

View(data)
















library(stringr)

data <- read.csv("D:/ds f/Handling Conractions.csv", stringsAsFactors = FALSE)

handle_emojis_emoticons <- function(text) {
  emoji_pattern <- "[\\U0001F600-\\U0001F64F\\U0001F300-\\U0001F5FF\\U0001F680-\\U0001F6FF\\U0001F700-\\U0001F77F]+"
  emoticon_pattern <- ":\\)|:-\\)|:\\(|:-\\(|;\\)|;-\\)|:D|:-D|XD|<3"
  
  emojis <- str_extract_all(text, emoji_pattern)
  emoticons <- str_extract_all(text, emoticon_pattern)
  
  text <- str_replace_all(text, emoji_pattern, "<EMOJI>")
  text <- str_replace_all(text, emoticon_pattern, "<EMOTICON>")
  
  return(list(
    modified_text = text,
    emojis = emojis,
    emoticons = emoticons
  ))
}

data$Processed_Review <- sapply(data$Review, function(review) {
  result <- handle_emojis_emoticons(review)
  result$modified_text
})

data$Review <- NULL

write.csv(data, "D:/ds f/Handling Emojis & Emoticons.csv", row.names = FALSE)

View(data)













library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(SnowballC)
library(hunspell)

data <- read.csv("D:/ds f/Handling Emojis & Emoticons.csv", stringsAsFactors = FALSE)

clean_text <- function(text) {
  if (is.na(text)) return(NA)
  text <- replace_html(text)  
  text <- tolower(text)      
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text)    
  return(text)
}

spell_check_text <- function(text) {
  if (is.na(text) || !is.character(text)) {
    return(text)
  }
  
  tokens <- unlist(strsplit(text, "\\s+"))  
  corrected_tokens <- sapply(tokens, function(word) {
    if (!hunspell_check(word)) {  
      suggestions <- hunspell_suggest(word)
      if (length(suggestions) > 0) {
        valid_suggestions <- suggestions[[1]][!grepl("[-\\s]", suggestions[[1]])] 
        if (length(valid_suggestions) > 0) {
          return(tolower(valid_suggestions[1]))  
        }
      }
      return("")  
    }
    return(tolower(word))  
  })
  
  corrected_tokens <- corrected_tokens[corrected_tokens != ""]
  return(paste(corrected_tokens, collapse = " "))
}

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

data_processed <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text))) %>%
  mutate(Processed_Review = sapply(Processed_Review, spell_check_text)) %>%
  mutate(across(everything(), ~ sapply(., format_tokens)))

write.csv(data_processed, "D:/ds f/Processed_Data_Complete.csv", row.names = FALSE)

View(data_processed)





















install.packages("topicmodels")

library(tm)
library(topicmodels)
library(dplyr)

data <- read.csv("D:/ds f/Processed_Data_Complete.csv", stringsAsFactors = FALSE)

reviews <- data$Processed_Review


head(data$Processed_Review)  
sum(data$Processed_Review == "")  



corpus <- Corpus(VectorSource(reviews))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))



dtm <- DocumentTermMatrix(corpus)

dtm <- removeSparseTerms(dtm, 0.99)

print(dim(dtm))

inspect(dtm[1:5, 1:5])  


num_topics <- 2  
lda_model <- LDA(dtm, k = num_topics, method = "Gibbs", control = list(seed = 1234))

top_terms <- terms(lda_model, 20) 
print("Top Terms in Each Topic:")
print(top_terms)

doc_topics <- topics(lda_model, 1) 
print("Document Topic Assignments:")
print(doc_topics)

topic_probabilities <- posterior(lda_model)$topics
print("Topic Probabilities for Each Document:")
print(topic_probabilities)


library(ggplot2)
library(tidytext)
install.packages("tidytext")
library(reshape2)


topic_terms <- tidy(lda_model, matrix = "beta")
top_terms <- topic_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = "Terms", y = "Beta", title = "Top Terms for Each Topic")

