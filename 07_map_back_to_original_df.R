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

# # Graph about topic proportion in dataset based on top topic per doc and retweets
# p <- joined_df |>
#   unnest(topic) |>
#   mutate(tot_docs = 483249) |>
#   left_join(topic_names, by = c("topic" = "id")) |>
#   group_by(name) |>
#   mutate(n = n(),
#          prop1 = n / tot_docs * 100) |>
#   select(name, prop1) |>
#   distinct() |>
#   bind_rows(count_top_doc, .id = "id") |> arrange(prop1) #|>
#   ggplot(aes(x = factor(name, levels = c(
#     "Human-tool skill comparison",
#     "Teams / Projects",
#     "Model specifications",
#     "Usage of the tool",
#     "Disruptiveness",
#     "Benefits of using the tool",
#     "Coding aid",
#     "Content creation",
#     "Hidden aspects of the tool",
#     "Economic impact",
#     "Business news",
#     "Future potential",
#     "Education",
#     "Job implications"
#   )), y = prop1)) +
#     geom_point(size = 2, alpha = 0.7, aes(color = id)) +
#   scale_color_brewer(palette = "Set1", name = "Dataset", labels = c("Retweet adjusted", "Model input"))+
#     coord_flip() +
#     xlab("") + ylab("Tweets (%)") +
#     theme_minimal()
#
#
# # Save the plot as a PNG file
# png(
#   "retweets_space_topics.png",
#   width = 5.5,
#   height = 2,
#   units = "in",
#   res = 300
# )
# print(p)
# dev.off()
