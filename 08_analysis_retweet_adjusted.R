# Libraries
library(tidyverse)
library(tidytext)
library(lubridate)
library(textdata)
library(cowplot)
library(RColorBrewer)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA COMES FROM THE R SOURCE FILE "map_back_to_original_df.R".
joined_df <- readRDS("~/chatgpt_paper/ChatGPT-and-GPT4-Twitter-discourse-review/files/joined_df.RDS")

top_topics_tokens2 <- joined_df %>%
  mutate(text = tolower(text_clean)) %>%
  unnest_tokens(word, text)

# associate number to name
topic_names <- tibble(
  id = as.character(c(1:14)),
  name = c(
    "Business reaction",
    "Interactions with the tool",
    "Usage for content creation",
    "Technology behind ChatGPT",
    "Human-ChatGPT labour/skill relationship",
    "Human reaction",
    "Negative aspects of the tool",
    "Usage in job seeking",
    "Usage in programming",
    "Education",
    "Business training initiatives",
    "Job implications",
    "Teams / Projects",
    "Political/ethical challenges"
  ), 
  domain = c("Big tech, human, and businesses reaction",
             "Technology and applications",
             "Technology and applications",
             "Technology and applications",
             "Implications and considerations",
             "Big tech, human, and businesses reaction",
             "Implications and considerations",
             "Technology and applications",
             "Technology and applications",
             "Technology and applications",
             "Big tech, human, and businesses reaction",
             "Implications and considerations",
             "Other",
             "Implications and considerations"
             )
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TOPIC PROPORTIONS READJUSTED
# Graph about topic proportion in dataset based on top topic per doc and retweets
p <- joined_df |>
  unnest(topic) |>
  mutate(tot_docs = 483249) |>
  left_join(topic_names, by = c("topic" = "id")) |>
  filter(topic != "13") |> 
  group_by(name) |>
  mutate(n = n(),
         prop1 = n / tot_docs * 100) |>
  select(name, prop1) |>
  distinct() |>
  bind_rows(count_top_doc, .id = "id") |> arrange(prop1) |> filter(name != "Teams / Projects") |> 
  ggplot(aes(x = factor(name, levels = c(
  "Human-ChatGPT labour/skill relationship",
  "Technology behind ChatGPT", 
  "Interactions with the tool",
  "Human reaction",
  "Usage in job seeking",
  "Usage in programming",
  "Usage for content creation",
  "Negative aspects of the tool",
  "Political/ethical challenges",
  "Business reaction",
  "Business training initiatives",
  "Education",
  "Job implications"
  )), y = prop1)) +
    geom_point(size = 2, alpha = 0.7, aes(color = id)) +
  scale_color_brewer(palette = "Set1", name = "Dataset", labels = c("Retweet adjusted", "Model input"))+
    coord_flip() +
    xlab("") + ylab("Tweets (%)") +
    theme_minimal()+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  theme(legend.position="top")


# Save the plot as a PNG file
png(
  "retweets_space_topics.png",
  width = 5.5,
  height = 3,
  units = "in",
  res = 300
)
print(p)
dev.off()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TOPIC ANALYSIS

# get mean topic proportions per 7-day bins
topic_proportion_per_month <- joined_df |>
  unnest(topic) |>
  filter(topic != 13) |> 
  left_join(topic_names, by = c("topic" = "id")) |> 
  mutate(
    date = as.Date(created_at),
    days_since_start = as.numeric(difftime(date, min(date), units = "days")),
    bin = floor(days_since_start / 7)
  ) |>
  group_by(bin) |>
  mutate(tot_week = n()) |>
  ungroup() |>
  group_by(bin, name) |>
  mutate(tot_week_topic = n(),
         prop = tot_week_topic / tot_week * 100) |>
  ungroup() |>
  select(date, bin, tot_week_topic, tot_week, prop, name, domain) |>
  distinct() |>
  arrange(bin)

# create a color palette that can accomodate 14 colors
colourCount = length(unique(topic_names$name))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# number of tweets by week and topic
topic_abs <- ggplot(topic_proportion_per_month, aes(x = date, y = tot_week_topic, fill = name)) +
  ylab("Number of tweets") +
  stat_smooth(
    geom = 'area',
    method = 'loess',
    span = 1 / 10,
    alpha = 2 / 3
  ) +
  scale_x_date(limits = c(
    min(topic_proportion_per_month$date),
    max(topic_proportion_per_month$date)
  ), expand = c(0, 0)) +
  scale_fill_manual(values = getPalette(colourCount), name = "") +
  theme_minimal() +
  facet_wrap( ~ name, ncol = 3) +
  theme(legend.position = "none")

# Save the plot as a PNG file
png(
  "topic_over_time_abs.png",
  width = 6,
  height = 6,
  units = "in",
  res = 300
)
print(topic_abs)
dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DOMAIN

# get mean domain proportions per 7-day bins
domain_proportion <- joined_df |>
  unnest(topic) |>
  filter(topic != 13) |> 
  left_join(topic_names, by = c("topic" = "id")) |> 
  mutate(
    date = as.Date(created_at),
    days_since_start = as.numeric(difftime(date, min(date), units = "days")),
    bin = floor(days_since_start / 7)
  ) |>
  group_by(bin) |>
  mutate(tot_week = n()) |>
  ungroup() |>
  group_by(bin, domain) |>
  mutate(tot_week_domain = n(),
         prop = tot_week_domain / tot_week * 100) |>
  ungroup() |>
  select(date, bin, tot_week_domain, tot_week, prop, name, domain) |>
  distinct() |>
  arrange(bin)

# domain prop plot
domain_prop <- ggplot(domain_proportion,
         aes(x = date, y = prop, fill = domain)) +
  ylab("number of tweets") +
  stat_smooth(
    geom = 'area',
    method = 'loess',
    span = 6 / 10,
    alpha = 0.7, position = "stack"
  ) +
  scale_x_date(limits = c(
    min(topic_proportion_per_month$date),
    max(topic_proportion_per_month$date)
  ), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2", name = "Domain") +
  xlab("")+ylab("Proportion of tweets")+
  theme_minimal() +
  theme(legend.position="top")

# domain absolute values plot
domain_abs <- ggplot(domain_proportion,
                      aes(x = date, y = tot_week_domain, fill = domain)) +
  ylab("number of tweets") +
  stat_smooth(
    geom = 'area',
    method = 'loess',
    span = 1 / 10,
    alpha = 0.6
  ) +
  scale_x_date(limits = c(
    min(topic_proportion_per_month$date),
    max(topic_proportion_per_month$date)
  ), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2", name = "Domain") +
  xlab("")+ylab("Number of tweets")+
  theme_minimal() +
  theme(legend.position="top")+
  geom_vline(xintercept = as.Date("2023-03-14"), linetype = 2) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

# Save the plot as a PNG file
png(
  "domain_absolute.png",
  width = 6,
  height = 3.5,
  units = "in",
  res = 300
)
print(domain_abs)
dev.off()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SENTIMENT ANALYSIS

# afinn (goes from -5 to 5)
afinn <- top_topics_tokens2 %>%
  unnest(topic) |>
  filter(topic != 13) |> 
  inner_join(get_sentiments("afinn")) %>%
  group_by(topic) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN") |> 
  arrange(sentiment) |> 
  rowid_to_column()

# bing and nrc (use positive/negative dichotomous variable)
bing_and_nrc <- bind_rows(
  top_topics_tokens2 %>%
    unnest(topic) |>
    filter(topic != 13) |> 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  top_topics_tokens2 %>%
    unnest(topic) |>
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c(
                   "positive",
                   "negative"
                 ))) %>%
    mutate(method = "NRC")
) %>%
  count(method, topic, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(sentiment = positive / (positive + negative) * 100)

# visualise results by topic
sentiment_dictionaries <- bind_rows(afinn,
                                    bing_and_nrc) %>%
  left_join(topic_names, by = c("topic" = "id"))

sd <- sentiment_dictionaries |> 
  filter(method == "AFINN") |> 
  ggplot(aes(sentiment, factor(name, levels = c(
    "Political/ethical challenges",
    "Job implications",
    "Business reaction",
    "Education",
    "Negative aspects of the tool",
    "Interactions with the tool",
    "Human-ChatGPT labour/skill relationship",
    "Technology behind ChatGPT",
    "Usage in job seeking",
    "Usage in programming",
    "Business training initiatives",
    "Human reaction",
    "Usage for content creation"
  )))) +
  geom_point(size = 3, alpha = 2/3, show.legend = FALSE, color = "lightslateblue") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  xlab("AFINN score") + ylab("")+
  scale_x_continuous(breaks = c(0, 40000))

sd2 <- sentiment_dictionaries |> 
  filter(method != "AFINN") |> 
  ggplot(aes(sentiment, factor(name, levels = c(
    "Political/ethical challenges",
    "Job implications",
    "Business reaction",
    "Education",
    "Negative aspects of the tool",
    "Interactions with the tool",
    "Human-ChatGPT labour/skill relationship",
    "Technology behind ChatGPT",
    "Usage in job seeking",
    "Usage in programming",
    "Business training initiatives",
    "Human reaction",
    "Usage for content creation"
  )), color = method)) +
  geom_point(size = 3, alpha = 2/3) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.y = element_blank())+
  xlab("Positive ratio") + ylab("")


sd3 <- plot_grid(sd, sd2, labels = "")


# Save the plot as a PNG file
png(
  "sentiment_by_topic_dictionaries1.png",
  width = 7.5,
  height = 4.25,
  units = "in",
  res = 300
)
print(sd3)
dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# NRC EMOTIONS

nrc <- top_topics_tokens2 %>%
  unnest(topic) |>
  filter(topic != 13) |> 
  inner_join(get_sentiments("nrc") %>%
               filter(!sentiment %in% c("positive",
                                        "negative"))) %>%
  count(topic, sentiment) |>
  left_join(topic_names, by = c("topic" = "id"))

emotion <- ggplot(nrc, aes(
    x = reorder(name, n),
    y = n,
    color = sentiment
  )) +
  geom_point(size = 3)+
  geom_line(aes(group = sentiment))+
  theme_minimal() +
  scale_color_manual(values = getPalette(colourCount), name = "emotion") +
  xlab("") + ylab("Number of words")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot as a PNG file
png(
  "emotion_by_topic.png",
  width = 7,
  height = 5.5,
  units = "in",
  res = 300
)
print(emotion)
dev.off()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SENTIMENTS TOPIC TIME

sentiment_df <- top_topics_tokens2 %>%
  inner_join(get_sentiments("afinn")) %>%
  unnest(topic) |>
  filter(topic != 13) |> 
  mutate(
    date = as.Date(created_at),
    days_since_start = as.numeric(difftime(date, min(date), units = "days")),
    bin = floor(days_since_start / 7)
  ) |>
  group_by(bin, topic) |>
  mutate(tot_week = mean(value)) |>
  ungroup() |>
  left_join(topic_names, by = c("topic" = "id")) |> 
  select(bin, date, tot_week, name) |> 
  distinct()

ggplot(sentiment_df, aes(x = bin, y = tot_week, fill = name)) +
  geom_bar(stat = "identity", alpha = 2 / 3, show.legend = FALSE) +
  scale_fill_manual(values = getPalette(colourCount)) +
  labs(x = "", y = "AFINN sentiment score") +
  theme_minimal() +
  facet_wrap( ~ name, ncol = 2)

s_w_t <-
ggplot(sentiment_df, aes(x = bin, y = tot_week, color = name)) +
  geom_line(show.legend = FALSE, aes(group = name), alpha = 1, linewidth = 1)+
  scale_color_manual(values = getPalette(colourCount), name = "Topic") +
  labs(x = "Week of data collection", y = "AFINN sentiment score") +
  theme_minimal() +
  facet_wrap( ~ name, ncol = 3)

# Save the plot as a PNG file
png(
  "sentiment_by_week_topic2.png",
  width = 6.5,
  height = 7,
  units = "in",
  res = 300
)
print(s_w_t)
dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SENTIMENT TIME (NO TOPIC)

over_time <- top_topics_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  unnest(topic) |>
  mutate(date = as.Date(created_at),
         week = format(date, "%G-W%V")) |>
  count(week, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(
    negative_ratio = negative / (positive + negative) * 100,
    positive_ratio = positive / (positive + negative) * 100
  ) |>
  arrange(desc(negative_ratio)) |>
  rowid_to_column() |>
  select(c(negative_ratio, positive_ratio, rowid, week)) |>
  pivot_longer(!c(rowid, week), names_to = "type")

over_time_sentiment <-
  ggplot(over_time, aes(x = week, y = value, group = type)) +
  stat_smooth(
    geom = 'area',
    method = 'loess',
    span = 3 / 10,
    alpha = 2 / 3,
    position = "stack",
    mapping = aes(fill = type)
  ) +
  scale_fill_brewer(
    palette = "Set1",
    name = "Sentiment",
    labels = c("Negative", "Positive")
  ) +
  labs(x = "Time", y = "Sentiment (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot as a PNG file
png(
  "sentiment_by_time.png",
  width = 7,
  height = 4,
  units = "in",
  res = 300
)
print(over_time_sentiment)
dev.off()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TIME INTERACTIVE
top_docs_joined <- top_docs_df |> 
  mutate(Topic = as.character(Topic)) |> 
  left_join(joined_df, by = join_by(query, created_at, author_id, conversation_id, id,
                                    in_reply_to_user_id, lang, dataset)) |> 
  group_by(Text_ID) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  left_join(topic_names, by = c("Topic" = "id")) |> 
  filter(Topic != "13") |> 
  select(text, Topic, Posterior, created_at, n_copies, name) |> 
  mutate(date = as.Date(created_at,'%Y-%m-%d')) |> 
  mutate(days_since_start = as.numeric(difftime(date, min(date), units = "days")))

write_csv(top_docs_joined, "top_docs_topic_timeline.csv")


