# DATA CLEANING SCRIPT

# Libraries
library(tidyverse)
library(qdapRegex)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data

full_dataset <- readRDS("~/chatgpt_employment_copy/files/full_dataset.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cleaning steps

# Select columns of interest
selected_data <- full_dataset |>
  select(text, query, created_at, author_id, conversation_id, id, in_reply_to_user_id, lang, dataset)

# Remove retweets
non_retweets <- selected_data |>
  filter(!str_detect(text, "^RT"))

# Filter only English tweets
english_tweets <- non_retweets |>
  filter(lang == "en")

# Define regular expression patterns to match
url_pattern <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+"
html_entity_pattern <- "&amp;|&lt;|&gt;"
rt_pattern <- "RT"
twitter_username_pattern <- "@[a-zA-Z0-9_]+"
hashtag_pattern <- "#[a-zA-Z0-9_]+"

# Combine patterns into a single regular expression
combined_pattern <- paste(url_pattern, html_entity_pattern, rt_pattern, "https", collapse = "|")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Build dataset with hashtags and mentions 

# Clean up the text
cleaned_data <- english_tweets |>
  mutate(
    text_clean = str_remove_all(text, combined_pattern),
    text_clean = rm_non_words(text_clean),
    text_clean = str_to_lower(text_clean),
    text_clean = rm_non_ascii(text_clean),
    text_clean = gsub("\\s+", " ", text_clean)
    
  )

# Group by cleaned text and keep only earliest tweet for each group; remove short tweets and irrelevant ones
unique_data <- cleaned_data |>
  group_by(text_clean) |>
  arrange(created_at) |>
  slice_head(n = 1) |>
  relocate(text_clean) |>
  filter(nchar(text_clean) > 2) |>
  filter(!str_detect(text_clean, pattern = paste(c("mev", "method", "ether"), collapse = "|")))

# Save object
# saveRDS(unique_data, "april_reclean_keep_hashtags_and_mentions.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Build dataset without hashtags and mentions 

# Clean up the text
cleaned_data <- english_tweets |>
  mutate(
    text_clean = str_remove_all(text, combined_pattern),
    text_clean = str_remove_all(text_clean, twitter_username_pattern),
    text_clean = str_remove_all(text_clean, hashtag_pattern),
    text_clean = rm_non_words(text_clean),
    text_clean = str_to_lower(text_clean),
    text_clean = rm_non_ascii(text_clean),
    text_clean = gsub("\\s+", " ", text_clean)
    
  )

# Group by cleaned text and keep only earliest tweet for each group; remove short tweets and irrelevant ones
unique_data <- cleaned_data |>
  group_by(text_clean) |>
  arrange(created_at) |>
  slice_head(n = 1) |>
  relocate(text_clean) |>
  filter(nchar(text_clean) > 2) |>
  filter(!str_detect(text_clean, pattern = paste(c("mev", "method", "ether"), collapse = "|")))

# Save object
# saveRDS(unique_data, "april_reclean_no_hashtags_or_mentions.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Timeline

# How does the discourse look like over time? We observe an inital peak in early December, followed by a dip around the holidays. The discourse then goes on to increasing and reaching new levels of intensity in the new year. This is the case both in the unique-tweets dataset and in the full dataset. Another thing we note is that retweets constantly outnumber original tweets.

filtered_data <- unique_data |> 
  mutate(gpt_vs_chatgpt = if_else(dataset == "gpt_data", "GPT4", "chatGPT"))

# plot with weekly count starting monday
my_plot <- filtered_data |> 
     ungroup() |> 
     mutate(week =  lubridate::floor_date(as.Date(created_at), "week")) |> 
     count(week, gpt_vs_chatgpt) |> 
     ggplot(aes(x = week, y = n, fill = gpt_vs_chatgpt)) +
   geom_bar(stat = "identity", position = "stack") +
   theme_minimal() +
   scale_fill_brewer(palette = "Set2", name = "Model") +
   xlab("Week")+
   ylab("Number of tweets")

# Save the plot as a PNG file
# png("my_plot.png", width = 5, height = , units = "in", res = 300)
# print(my_plot)
# graphics.off()

# plot with rolling 7 day count starting on day 1 of data collection
filtered_data |> 
  ungroup() |> 
  mutate(
    date = as.Date(created_at),
    days_since_start = as.numeric(difftime(date, min(date), units = "days")),
    bin = floor(days_since_start / 7)
  ) |>
  count(bin, gpt_vs_chatgpt) |> 
  ggplot(aes(x = bin, y = n, fill = gpt_vs_chatgpt)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Model") +
  xlab("Week")+
  ylab("Number of tweets")+
  scale_x_continuous(breaks = seq(0,18,4.5), labels = c("Dec","Jan", "Feb", "Mar","Apr"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Query analysis

# Which queries returned more tweets? Some queries are more popular than other (and they might also contain more noise). The trends are similar for both the filtered and the full datasets, with the most popular query using the word "work", followed by "education" and "job" or "jobs".

query_index_filtered <- full_dataset |> 
ungroup() |>
count(query) 

query_index_fulldf <- unique_data |> 
ungroup() |>
count(query) 

queries <- bind_rows(query_index_filtered, query_index_fulldf, .id = "id")

queries |> 
ggplot(aes(x = reorder(query,n), y = n, color = id)) +
geom_point(size = 2, alpha = 0.7) +
theme_minimal() +
scale_color_brewer(palette = "Set2", labels = c("Filtered", "Full"), name = "Dataset") +
xlab("query") +
ylab("number of tweets") +
coord_flip()

# The least popular queries are still not empty, returning some results even when not counting retweets.
unique_data |> 
ungroup() |>
count(query, sort = TRUE) |>
slice_tail(n = 5)

# Do different queries return different tweets? We find that a total of 41169 tweets occur in multiple queries. However, the majority, 457545 tweets, occur in one query only.
query_cooccurrences <- full_dataset |>
group_by(text, query, author_id) |>
count(sort = TRUE)

qq <- query_cooccurrences |>
ungroup() |>
group_by(text, author_id) |>
count(sort = TRUE) 

ggplot(qq, aes(x = n)) +
geom_bar(fill = "lightslateblue") +
theme_minimal()

print(nrow(filter(qq, n == 1))) # number of tweets in only one query
nrow(filter(qq, n > 1)) # number of tweets in more than one query
