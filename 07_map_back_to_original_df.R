# This script takes the topic labelled dataset and links it back to the full df, to quantify topics when accounting for retweets

# Libraries
library(qdapRegex)
library(tidyverse)

# Data
full_dataset <-
  readRDS("~/chatgpt_employment_copy/scripts/full_dataset.RDS")

# Select columns of interest
selected_data <- full_dataset %>%
  select(
    text,
    query,
    created_at,
    author_id,
    conversation_id,
    id,
    in_reply_to_user_id,
    lang,
    dataset
  )

# # Remove retweets
# non_retweets <- selected_data %>%
#   filter(!str_detect(text, "^RT"))

# Filter only English tweets
english_tweets <- selected_data %>%
  filter(lang == "en")

# Define regular expression patterns to match
url_pattern <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+"
html_entity_pattern <- "&amp;|&lt;|&gt;"
rt_pattern <- "RT"

# Combine patterns into a single regular expression
combined_pattern <-
  paste(url_pattern,
        html_entity_pattern,
        rt_pattern,
        "https",
        collapse = "|")

# Define regular expression patterns to match
twitter_username_pattern <- "@[a-zA-Z0-9_]+"
hashtag_pattern <- "#[a-zA-Z0-9_]+"

# Clean up the text
cleaned_data <- english_tweets %>%
  mutate(
    text_clean = str_remove_all(text, combined_pattern),
    text_clean = str_remove_all(text_clean, twitter_username_pattern),
    text_clean = str_remove_all(text_clean, hashtag_pattern),
    text_clean = rm_non_words(text_clean),
    text_clean = str_to_lower(text_clean),
    text_clean = rm_non_ascii(text_clean),
    text_clean = gsub("\\s+", " ", text_clean),
    text_clean = str_remove(text_clean, "^rt ") # remove RT tag
    
  )

# Group by cleaned text and keep only earliest tweet for each group; remove short tweets and irrelevant ones
unique_data <- cleaned_data %>%
  group_by(text_clean) %>%
  arrange(created_at) %>%
  mutate(n_copies = n()) %>%
  select(text_clean, n_copies) %>%
  filter(nchar(text_clean) > 2) %>%
  ungroup() |>
  filter(!str_detect(text_clean, pattern = paste(c(
    "mev", "method", "ether"
  ), collapse = "|")))


# join
joined_df <-
  left_join(unique_data, data1, by = c("text_clean" = "text")) |> filter(!is.na(doc_id)) |>
  left_join(toptopics, by = c("text_clean" = "text", "doc_id"))

saveRDS(joined_df, "joined_df.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# TOP RETWEETS
top_retweets <- joined_df |> group_by(text_clean) |> sample_n(size = 1) |> ungroup() |> arrange(desc(n_copies)) |> slice_head(n = 100)

top_retweets |> 
  ungroup() |> ggplot(aes(x = n_copies)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)+
  scale_x_log10()+
  theme_minimal()

library(googlesheets4)
write_sheet(top_retweets, ss = "https://docs.google.com/spreadsheets/d/1650HIlq1o1z4zMl9GvlGxmPineL3y44xOOeX_Z-I3wE/edit#gid=251884392", sheet = "top_retweets")
