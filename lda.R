library("dplyr")
library("stringr")
library("tidyr")
library("tidytext")
library("topicmodels")
library("ggplot2")
library("rvest")
library("scales")

mobile <- read.csv(file = "mobile.csv",row.names = 1,stringsAsFactors = FALSE)
book <- read.csv(file = "book.csv",row.names = 1,stringsAsFactors = FALSE)
ppowder <- read.csv(file = "protienpowder.csv",row.names = 1,stringsAsFactors = FALSE)
trimmer <- read.csv(file = "trimmer.csv",row.names = 1,stringsAsFactors = FALSE)

addcolumn<- function(data,topic){
  data$review_id <-0
  for(i in 1:nrow(data)){
    data$review_id[i] <- paste0(topic,"_",i)
  }
  return(data)
}

mobile<- addcolumn(mobile,"mobile")
book<-addcolumn(book,"book")
ppowder<- addcolumn(ppowder,"ppowder")
trimmer<-addcolumn(trimmer,"trimmer")

all_reviews <- rbind(mobile,book,ppowder,trimmer)
names(all_reviews)[names(all_reviews) == "x"] <- "review_text"
#guess_encoding(all_reviews[,1])
repair_encoding(all_reviews[,1])

# split into words
all_reviews_word <- all_reviews %>%
  unnest_tokens(word, review_text)

# finding word count
word_counts <- all_reviews_word %>%
  anti_join(stop_words) %>%
  count(review_id, word, sort = TRUE) %>%
  ungroup()

word_counts

# creating dtm of reviews 
reviews_dtm <- word_counts %>%
  cast_dtm(review_id, word, n)

reviews_dtm
reviews_lda <- LDA(reviews_dtm, k = 4, control = list(seed = 1234))
reviews_lda

review_topics <- tidy(reviews_lda, matrix = "beta")
review_topics

top_terms <- review_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# per document classification
reviews_gamma <- tidy(reviews_lda, matrix = "gamma")
reviews_gamma

reviews_gamma <- reviews_gamma %>%
  separate(document, c("title", "review_id"), sep = "_", convert = TRUE)

reviews_gamma

reviews_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

review_classifications <- reviews_gamma %>%
  group_by(title, review_id) %>%
  top_n(1, gamma) %>%
  ungroup()

review_classifications

review_topics <- review_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

review_classifications %>%
  inner_join(review_topics, by = "topic") %>%
  filter(title != consensus)

assignments <- augment(reviews_lda, data = reviews_dtm)
assignments

assignments <- assignments %>%
  separate(document, c("title", "review_id"), sep = "_", convert = TRUE) %>%
  inner_join(review_topics, by = c(".topic" = "topic"))

assignments

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "review words were assigned to",
       y = "review words came from",
       fill = "% of assignments")

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))
