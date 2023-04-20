# Download and install necessary packages
install.packages("tidyverse")
install.packages("tidytext")
install.packages("tm")
install.packages("topicmodels")
install.packages("reshape2")
install.packages("irr")
library(tidyverse) # to work with data in tidy format
library(tidytext) # to work with text data in tidy format
library(tm) # to create dtm
library(topicmodels) # for the LDA model (and other implementations)
library(reshape2)
library(irr) # to compute intercoder reliability / check the overlap between predictions and "ground truth"


# Import our data set (Data set originally created for https://brill.com/view/journals/gg/28/4/article-p486_2.xml)

articles <- read.csv("/cloud/project/week07_articles_dataset_example.csv")
articles <- as_tibble(articles)
View(articles)

# How many topics did we assign to the articles?
length(unique(articles$Topic))
table(articles$Topic)

# Pre-process the title and abstracts, unnest the tokens, remove stopwords
tokens <- articles %>%
  unite(col = "text", Title, Abstract) %>%
  select(text) %>%
  rowid_to_column(var = "doc_id") %>% # adds a unique doc_id to each article
  unnest_tokens(output = word, input = text, token = "words", to_lower = T) %>%
  anti_join(stop_words)

word_counts <- tokens %>%
  count(doc_id, word, sort = TRUE)

word_counts

# Topicmodels requires a DocumentTermMatrix, not tidytext format (one-token-per-row)
articles_dtm <- word_counts %>%
  cast_dtm(doc_id, word, n)
articles_dtm

# Now we can use the LDA() function to fit a k-topic model. 
# From our human coders, we know we're looking for 10 topics.

articles_lda <- LDA(articles_dtm, k = 10, control = list(seed = 123))
articles_lda

# Let's have a look at the model's predictions.
# Beta describes the probability of a term being generated from a topic.
articles_beta <- tidy(articles_lda, matrix = "beta")
head(articles_beta)
tail(articles_beta)

# Check out the top 3 terms per topic
top_terms <- articles_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 3) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# Visualize them
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

topics <- tibble(
  topic = c(1:10),
  topic_name = c("Environment", "Environment", "General", "Environment", "Economic", "Health", "General", "Economic", "General", "General")
  )

# Get article predictions

articles_gamma <- tidy(articles_lda, matrix = "gamma")
articles_gamma

# Keep only the highest predicted class for each document, rename topics
articles_classifications <- articles_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup() %>%
  left_join(topics, by = "topic")
articles_classifications

articles_classifications$topic_name %>% table() # our ML predictions
table(articles$Topic) # the original assignments

# Check accuracy and validate: compute agreement between both approaches
bind_cols(original = articles$Topic, predicted = articles_classifications$topic_name) %>% 
  agree()

bind_cols(original = articles$Topic, predicted = articles_classifications$topic_name) %>% 
  filter(predicted %in% c("Economic", "Environment")) %>% 
  agree()
