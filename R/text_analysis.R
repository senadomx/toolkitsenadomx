#install.packages("tidytext")
library(tidytext)
library(dplyr)
library(datasenadomx)
library(topicmodels)
library(tm)
library(tidyr)
library(ggplot2)
data(l62rollcalls)
head(l62rollcalls)

stpwrds <- stopwords("spanish") %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  c(c("proyecto", "legislativos", "comisiones"))

words <- l62rollcalls %>%
  select(rollcall_id, texto) %>%
  unnest_tokens(palabra, texto) %>%
  filter(! palabra %in% stpwrds) %>%
  count(rollcall_id, palabra, sort = TRUE) %>%
  ungroup()
total_words <- words %>%
  group_by(rollcall_id) %>%
  summarise(total = sum(n))
words_freq <- left_join(words, total_words, by = "rollcall_id")  %>%
  mutate(freq = n / total)

bigrams <- l62rollcalls %>%
  select(rollcall_id, texto) %>%
  unnest_tokens(bigrama, texto, token = "ngrams", n = 2) %>%
  separate(bigrama, c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(! word1 %in% stpwrds, ! word2 %in% stpwrds) %>%
  count(rollcall_id, bigrama, word1, word2, sort = TRUE) %>%
  ungroup()
total_bigrams <- bigrams %>%
  group_by(rollcall_id) %>%
  summarise(total = sum(n))
bigrams_freq <- left_join(bigrams, total_bigrams, by = "rollcall_id") %>%
  mutate(freq = n / total)

word_dtm <- words_freq %>%
  cast_dtm(rollcall_id, palabra, total)
word_lda <- LDA(word_dtm, k = 5, control = list(seed = 1234))

word_perterm <- tidy(word_lda, matrix = "beta")
top_terms <- word_perterm %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

word_perrc <- tidy(word_lda, matrix = "gamma")
word_perrc %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)




bigram_dtm <- bigrams_freq %>%
  cast_dtm(rollcall_id, bigrama, total)
bigram_lda <- LDA(bigram_dtm, k = 8, control = list(seed = 1234))

bigram_perterm <- tidy(bigram_lda, matrix = "beta")
top_terms <- bigram_perterm %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

