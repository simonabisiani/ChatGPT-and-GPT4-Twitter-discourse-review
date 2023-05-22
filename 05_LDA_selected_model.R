# This script fits LDA(k = 14) on the pruned, no hashtag dataset and generates figures for the article

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries
library(ldatuning)
library(topicmodels)
library(tidyverse)
library(tm)
library(tidytext)
library(topicdoc)
library(quanteda)
library(LDAvis)
library(RColorBrewer)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data
no_hashtags_or_mentions <-
  readRDS("scripts/april_reclean_no_hashtags_or_mentions.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA PREP AND MODEL FITTING
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Preprocessing (let's repeat the steps from script 03)
data <-
  no_hashtags_or_mentions |>
  select(text_clean) |>
  rowid_to_column() |>
  rename(text = text_clean,
         doc_id = rowid) |>
  ungroup()

# create corpus
doc.corpus <- corpus(data,
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
  tokens_remove(pattern = c(stopwords("en"),
                            "chatgpt", "ai", "gt", "gpt"))

# make a sparse document feature matrix and trim it to remove common words and retain unique words
dfmat <- dfm(tokens) |>
  dfm_select(min_nchar = 3) |>
  dfm_trim(
    min_termfreq = 0.7,
    # keeping the top 30% of terms based on term frequency
    termfreq_type = "quantile",
    # terms whose frequency fall below the specified quantile (in this case 70%) will be removed
    max_docfreq = 0.15,
    # a term should not appear in more than 15% of the documents
    docfreq_type = "prop"
  )

# Convert dfmat to dtm
dtm <- convert(dfmat, to = "topicmodels")

# Fit LDA
lda_output <-
  LDA(dtm,
      k = 14,
      control = list(seed = 9161),
      method = "Gibbs")

# Observe coherence
mean(topic_coherence(
  lda_output,
  dtm,
  top_n_tokens = 10,
  smoothing_beta = 1
))

# Observe exclusivity
# mean(topic_exclusivity(lda_output, top_n_tokens = 10))

# Visualise intertopic distance map
dtm = dtm[slam::row_sums(dtm) > 0, ]
phi <- as.matrix(posterior(lda_output)$terms)
theta <- as.matrix(posterior(lda_output)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(
  phi = phi,
  theta = theta,
  vocab = vocab,
  R = 15,
  doc.length = doc.length,
  term.frequency = term.freq
)
serVis(json)

# Right way:
new.order <- RJSONIO::fromJSON(json)$topic.order

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MODEL INTERPRETATION - BETA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Look at beta, per-topic-per-word probabilities, word-topic-matrix
topics <- tidy(lda_output, matrix = "beta")
top_terms <- topics |>
  group_by(topic) |>
  slice_max(beta, n = 15) |>
  ungroup() |>
  arrange(topic, -beta) |>
  select(-beta) |>
  pivot_wider(names_from = topic,
              values_from = term,
              values_fn = list) |>
  unnest_longer(c(1:14))

# write_csv(top_terms, "top_terms_LDA14.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DOCUMENT-TOPIC PROBABILITIES
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Look at per-document-per-topic probabilities: γ (“gamma”), document-topic-matrix
gammaDF <- as.data.frame(lda_output@gamma)
names(gammaDF) <- c(1:14)

# Graph about topic proportion in dataset based on sum of proportions of topics within documents
prop_topic_df <- gammaDF |>
  mutate(across(1:14, sum)) |>
  distinct() |>
  rename(
    "Business news" = 1,
    "Usage of the tool" = 2,
    "Content creation" = 3,
    "Model specifications" = 4,
    "Human-tool skill comparison" = 5,
    "Disruptiveness" = 6,
    "Hidden aspects of the tool" = 7,
    "Benefits of using the tool" = 8,
    "Coding aid" = 9,
    "Education" = 10,
    "Future potential" = 11,
    "Job implications" = 12,
    "Teams / Projects" = 13,
    "Economic impact" = 14
  ) |>
  pivot_longer(cols = 1:14) |>
  mutate(tot = sum(value),
         prop = value / tot * 100) |>
  arrange(prop)

# associate number to name
topic_names <- tibble(
  id = as.character(c(1:14)),
  name = c(
    "Business news",
    "Usage of the tool",
    "Content creation",
    "Model specifications",
    "Human-tool skill comparison",
    "Disruptiveness",
    "Hidden aspects of the tool",
    "Benefits of using the tool",
    "Coding aid",
    "Education",
    "Future potential",
    "Job implications",
    "Teams / Projects",
    "Economic impact"
  )
)

set.seed(38474)
# how spread out are topics in a random sample of 50 documents?
gamma_plot <- gammaDF |>
  sample_n(size = 50) |>
  rowid_to_column() |>
  pivot_longer(cols = 2:15, names_to = "topic") |>
  left_join(topic_names, by = c("topic" = "id")) |>
  ggplot(aes(rowid, name)) +
  geom_raster(aes(fill = value)) +
  theme_minimal() +
  xlab("Sample of 50 documents") +
  ylab("Topic") +
  scale_x_discrete(breaks = seq(1, 51, 1))

# # Save the plot as a PNG file
# png(
#   "gamma_plot.png",
#   width = 7,
#   height = 2,
#   units = "in",
#   res = 300
# )
# print(gamma_plot)
# dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TOP TOPIC PER DOCUMENT
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Now for each doc, find just the top-ranked topic
toptopics <- as.data.frame(cbind(
  document = row.names(gammaDF),
  text_id = lda_output@documents,
  topic = apply(gammaDF, 1, function(x)
    names(gammaDF)[which(x == max(x))])
)) # how to retain gamma?

# Calculate the row sums of dfmat
row_sums <- rowSums(dfmat)

# Identify the documents with zero features
zero_feature_docs <- names(row_sums[row_sums == 0])
data_after_removing_zero_feature_docs <- subset(data1,!(rownames(data1) %in% zero_feature_docs))
toptopics <- cbind(data_after_removing_zero_feature_docs, toptopics)

# append date information
toptopics <- toptopics |>
  left_join(no_hashtags_or_mentions, by = c("text" = "text_clean"))

# library(googlesheets4)
# write_sheet(toptopics, ss = "https://docs.google.com/spreadsheets/d/1650HIlq1o1z4zMl9GvlGxmPineL3y44xOOeX_Z-I3wE/edit#gid=251884392", sheet = "main_topic_per_tweet_full_df")

# Graph about topic proportion in dataset based on top topic per doc
count_top_doc <- toptopics |>
  unnest(topic) |>
  mutate(tot_docs = 250510) |>
  left_join(topic_names, by = c("topic" = "id")) |>
  group_by(name) |>
  mutate(n = n(),
         prop1 = n / tot_docs * 100) |>
  select(name, prop1) |>
  distinct()

colourCount = length(unique(topic_names$name))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

top_props <-
  ggplot(count_top_doc, aes(x = reorder(name, prop1), y = prop1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") + ylab("Tweets (%)") +
  theme_minimal() +
  scale_y_continuous(limits = c(5, 10))

# # Save the plot as a PNG file
# png(
#   "topic_props.png",
#   width = 6,
#   height = 2,
#   units = "in",
#   res = 300
# )
# print(top_props)
# dev.off()

# NOW SELECT SAMPLE FROM EACH TOPIC
toptopic_sample <- toptopics |>
  select(-c(document, text_id)) |>
  group_by(topic) |>
  slice_sample(prop = 0.01) |>
  unnest(cols = topic) |>
  arrange(as.numeric(topic))

# library(googlesheets4)
# write_sheet(toptopic_sample, ss = "https://docs.google.com/spreadsheets/d/1650HIlq1o1z4zMl9GvlGxmPineL3y44xOOeX_Z-I3wE/edit#gid=251884392", sheet = "toptopic_per_doc_sample")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TOP DOCUMENTS PER TOPIC
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Look at the most representative documents for each topic
topic.docs = posterior(lda_output)$topics[, 1]
topic.docs = sort(topic.docs, decreasing = T)
head(topic.docs)
topdoc = names(topic.docs)[1:10]
corpus2 <-
  corpus(data_after_removing_zero_feature_docs, text_field = "text")
topdoc_corp = corpus2[docnames(corpus2) %in% topdoc]
as.character(topdoc_corp)

# Function to get top documents for a given topic
get_top_documents <- function(lda_output, corpus, topic, n = 500) {
  topic.docs <- posterior(lda_output)$topics[, topic]
  topic.docs <- sort(topic.docs, decreasing = TRUE)
  top_doc_names <- names(topic.docs)[1:n]
  top_doc_values <- topic.docs[1:n]
  top_docs <- corpus[docnames(corpus) %in% top_doc_names]
  data.frame(
    Text_ID = docnames(top_docs),
    Text = as.character(top_docs),
    Topic = topic,
    Rank = 1:n,
    Posterior = top_doc_values,
    stringsAsFactors = FALSE
  )
}

# Apply the function to all topics
num_topics <- 14
top_docs_df <- lapply(1:num_topics, function(topic) {
  get_top_documents(lda_output, corpus2, topic)
})

# Combine results into a single data frame
top_docs_df <- do.call(rbind, top_docs_df)

top_docs_df <- top_docs_df |>
  left_join(no_hashtags_or_mentions, by = c("Text" = "text_clean"))

# library(googlesheets4)
# write_sheet(top_docs_df, ss = "https://docs.google.com/spreadsheets/d/1650HIlq1o1z4zMl9GvlGxmPineL3y44xOOeX_Z-I3wE/edit#gid=251884392", sheet = "most_representative_docs_per_topic")
#
# save.image("LDA_selected_model.RData")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TOPICS OVER TIME
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# get mean topic proportions per 7-day bins
topic_proportion_per_month <- toptopics |>
  unnest(topic) |>
  mutate(
    date = as.Date(created_at),
    days_since_start = as.numeric(difftime(date, min(date), units = "days")),
    bin = floor(days_since_start / 7)
  ) |>
  group_by(bin) |>
  mutate(tot_week = n()) |>
  ungroup() |>
  group_by(bin, topic) |>
  mutate(tot_week_topic = n(),
         prop = tot_week_topic / tot_week * 100) |>
  ungroup() |>
  select(date, bin, tot_week_topic, tot_week, prop, topic) |>
  distinct() |>
  arrange(bin)

topic_proportion_per_month <- topic_proportion_per_month |>
  left_join(topic_names, by = c("topic" = "id"))

topic_absolute_values <-
  ggplot(topic_proportion_per_month,
         aes(x = date, y = tot_week_topic, fill = name)) +
  ylab("number of tweets") +
  stat_smooth(
    geom = 'area',
    method = 'loess',
    span = 1 / 10,
    alpha = 1 / 3
  ) +
  scale_fill_manual(values = getPalette(colourCount)) +
  scale_x_date(limits = c(
    min(topic_proportion_per_month$date),
    max(topic_proportion_per_month$date)
  ), expand = c(0, 0)) +
  theme_minimal() +
  facet_wrap( ~ name, ncol = 2) +
  theme(legend.position = "none")

# # Save the plot as a PNG file
# png(
#   "topic_over_time_absolute.png",
#   width = 5,
#   height = 6,
#   units = "in",
#   res = 300
# )
# print(topic_absolute_values)
# dev.off()

# PROPORTION INSTEAD OF ABSOLUTE VALUES 
topic_prop_values <-
  ggplot(topic_proportion_per_month, aes(x = date, y = prop, fill = name)) +
  ylab("Topic proportion (%)") +
  stat_smooth(
    geom = 'area',
    method = 'loess',
    span = 1 / 10,
    alpha = 1 / 3
  ) +
  scale_fill_manual(values = getPalette(colourCount)) +
  scale_x_date(limits = c(
    min(topic_proportion_per_month$date),
    max(topic_proportion_per_month$date)
  ), expand = c(0, 0)) +
  theme_minimal() +
  facet_wrap( ~ name, ncol = 2) +
  theme(legend.position = "none")

# # Save the plot as a PNG file
# png(
#   "topic_over_time_prop.png",
#   width = 5.5,
#   height = 6.8,
#   units = "in",
#   res = 300
# )
# print(topic_prop_values)
# dev.off()
