library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(tidyr)
library(tm)
library(topicmodels)
library(broom)
library(purrr)
library(lubridate)
library(readr)
library(ggplot2)

speeches <- read_file('../trump_speeches/data/full_speech.txt')
speeches <- as_tibble(cbind(source = 'trump_speeches', text = speeches))

# unnest trump speech corpus by word, remove stop words
tidy_trump <- speeches %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# most common words
tidy_trump %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('word') +
  ylab('count') +
  ggtitle('Most common words in 51 Trump campaign speeches') +
  coord_flip()

word_count <- tidy_trump %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

# unnest trump whitehouse.gov corpus by bigram, remove stop words
tidy_trump_bigrams <- speeches %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(bigram, word1, word2, sep = ' ')

# most common bigrams
tidy_trump_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('bigram') +
  ylab('count') +
  ggtitle('Most common bigrams i 51 Trump campaign speeches') +
  coord_flip()

bigram_count <- tidy_trump_bigrams %>%
  count(bigram, sort = TRUE) %>%
  mutate(bigram = reorder(bigram, n))

# unnest trump whitehouse.gov corpus by trigram, remove stop words
tidy_trump_trigram <- speeches %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word3 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(trigram, word1, word2, word3, sep = ' ')

# most common trigrams
tidy_trump_trigram %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = trigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('trigram') +
  ylab('count') +
  ggtitle('Most common trigrams on whitehouse.gov on January 25, 2017') +
  coord_flip()

trigram_count <- tidy_trump_trigram %>%
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))
