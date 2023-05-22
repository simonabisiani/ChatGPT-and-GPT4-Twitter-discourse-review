# LOOKING FOR K
# 01/05/2023
# Simona Bisiani

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Four different dataset combinations:
# 1. Without hashtags / mentions - unpruned
# 2. Without hashtags / mentions - pruned
# 3. With hashtags / mentions - unpruned
# 4. With hashtags / mentions - pruned

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries 
library(tidyverse)
library(ldatuning)
library(topicmodels)
library(tidyverse)
library(tm)
library(tidytext)
library(quanteda)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load script results that were saved after running this script 
# load("LDA_find_topic_range.RData")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data

no_hashtags_or_mentions <-
  readRDS("scripts/april_reclean_no_hashtags_or_mentions.RDS")

with_hashtags_and_mentions <-
  readRDS("scripts/april_reclean_keep_hashtags_and_mentions.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Without hashtags / mentions - unpruned

data1 <-
  no_hashtags_or_mentions |>
  select(text_clean) |>
  rowid_to_column() |>
  rename(text = text_clean,
         doc_id = rowid) |>
  ungroup()

# split into words
tokens <- data1 |>
  unnest_tokens(word, text)

# find document-word counts
word_counts <- tokens |>
  anti_join(stop_words) |>
  filter(!word %in% c("chatgpt", "ai", "gt", "gpt")) |>
  count(doc_id, word, sort = TRUE)

# compute document term matrix
dtm1 <- word_counts |>
  cast_dtm(doc_id, word, n)

# find ideal number of topics (or topic range)
result1 <- FindTopicsNumber(
  dtm1,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 16L,
  verbose = TRUE
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Without hashtags / mentions - pruned

# create corpus
doc.corpus <- corpus(data1,
                     text_field = "text")

# extract tokens
tokens <- tokens(
  doc.corpus,
  remove_url = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE,
  padding = FALSE
) |>
  tokens_remove(pattern = c(
    stopwords("en"),
    "chatgpt", "ai", "gt", "gpt"
  ))

# make a sparse document feature matrix and trim it to reduce its size
dfmat <- dfm(tokens) |>
  dfm_select(min_nchar = 3) |>
  dfm_trim(
    min_termfreq = 0.7, # keeping the top 30% of terms based on term frequency
    termfreq_type = "quantile", # terms whose frequency fall below the specified quantile (in this case 70%) will be removed
    max_docfreq = 0.15, # a term should not appear in more than 15% of the documents
    docfreq_type = "prop"
  )

dtm2 <- convert(dfmat, to = "topicmodels")

# find ideal number of topics (or topic range)
result2 <- FindTopicsNumber(
  dtm2,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 16L,
  verbose = TRUE
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. With hashtags / mentions - unpruned

data2 <-
  with_hashtags_and_mentions |>
  select(text_clean) |>
  rowid_to_column() |>
  rename(text = text_clean,
         doc_id = rowid) |>
  ungroup()

# split into words
tokens <- data2 |>
  unnest_tokens(word, text)

# find document-word counts
word_counts <- tokens |>
  anti_join(stop_words) |>
  filter(!word %in% c("chatgpt", "ai", "gt", "gpt")) |>
  count(doc_id, word, sort = TRUE)

# compute document term matrix
dtm3 <- word_counts |>
  cast_dtm(doc_id, word, n)

result3 <- FindTopicsNumber(
  dtm3,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 16L,
  verbose = TRUE
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. With hashtags / mentions - pruned

# create corpus
doc.corpus <- corpus(data2,
                     text_field = "text")

# extract tokens
tokens <- tokens(
  doc.corpus,
  remove_url = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE,
  padding = FALSE
) |>
  tokens_remove(pattern = c(
    stopwords("en"),
    "chatgpt", "ai", "gt", "gpt"
  ))

# make a sparse document feature matrix and trim it to reduce its size, particularly we remove common words and retain unique words
dfmat <- dfm(tokens) |>
  dfm_select(min_nchar = 3) |>
  dfm_trim(
    min_termfreq = 0.7, # keeping the top 30% of terms based on term frequency
    termfreq_type = "quantile", # terms whose frequency fall below the specified quantile (in this case 80%) will be removed
    max_docfreq = 0.15, # a term should not appear in more than 15% of the documents
    docfreq_type = "prop"
  )

dtm4 <- convert(dfmat, to = "topicmodels")

result4 <- FindTopicsNumber(
  dtm4,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 16L,
  verbose = TRUE
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Compare results from different models

FindTopicsNumber_plot(result1)
# Save the plot as a PNG file
ggsave("topic_metrics_1.png",
               width = 8,
               height = 4,
               units = "in",
               dpi = 300)

FindTopicsNumber_plot(result2)
# Save the plot as a PNG file
ggsave("topic_metrics_2.png",
       width = 8,
       height = 4,
       units = "in",
       dpi = 300)

FindTopicsNumber_plot(result3)
# Save the plot as a PNG file
ggsave("topic_metrics_3.png",
       width = 8,
       height = 4,
       units = "in",
       dpi = 300)


FindTopicsNumber_plot(result4)
# Save the plot as a PNG file
ggsave("topic_metrics_4.png",
       width = 8,
       height = 4,
       units = "in",
       dpi = 300)
