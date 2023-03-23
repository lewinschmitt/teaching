## This mini-tutorial is based on Tidytext: https://www.tidytextmining.com/tfidf.html

library(dplyr) 
library(janeaustenr) # Jane Austen books
library(tidytext) # for text data according to tidyverse principles
library(ggplot2) # for visualisation
library(forcats) # good to work with categorical data (factors)

View(austen_books() %>% head(n =30))

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

# Let's count how many words there are per book
total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
total_words

book_words <- left_join(book_words, total_words)

book_words

# Let's visualise the term frequency:

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Zipf's law: the frequency that a word appears is inversely proportional to its rank.

# let's get the rank
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

# let's visualize this:

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# Words that matter: tf-idf

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
