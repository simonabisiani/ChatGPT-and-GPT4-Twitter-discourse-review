# Libraries
library(tidyverse)
library(tidytext)
library(lubridate)
library(textdata)
library(cowplot)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DATA COMES FROM THE R SOURCE FILE "map_back_to_original_df.R".
joined_df <- readRDS("joined_df.RDS")

top_topics_tokens2 <- joined_df %>%
  mutate(text = tolower(text_clean)) %>%
  unnest_tokens(word, text)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SENTIMENT ANALYSIS

# afinn (goes from -5 to 5)
afinn <- top_topics_tokens2 %>%
  unnest(topic) |>
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
  ggplot(aes(sentiment, factor(
    name,
    levels = c(
      "Economic impact",
      "Job implications",
      "Business news",
      "Education",
      "Hidden aspects of the tool",
      "Usage of the tool",
      "Benefits of using the tool",
      "Human-tool skill comparison",
      "Model specifications",
      "Coding aid",
      "Future potential",
      "Disruptiveness",
      "Content creation",
      "Teams / Projects"
    )
  ))) +
  geom_point(size = 3, alpha = 2/3, show.legend = FALSE, color = "lightslateblue") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  xlab("AFINN score") + ylab("")+
  scale_x_continuous(breaks = c(0, 40000))

sd2 <- sentiment_dictionaries |> 
  filter(method != "AFINN") |> 
  ggplot(aes(sentiment, factor(
    name,
    levels = c(
      "Economic impact",
      "Job implications",
      "Business news",
      "Education",
      "Hidden aspects of the tool",
      "Usage of the tool",
      "Benefits of using the tool",
      "Human-tool skill comparison",
      "Model specifications",
      "Coding aid",
      "Future potential",
      "Disruptiveness",
      "Content creation",
      "Teams / Projects"
    )
  ), color = method)) +
  geom_point(size = 3, alpha = 2/3) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.y = element_blank())+
  xlab("Positive ratio") + ylab("")


sd3 <- plot_grid(sd, sd2, labels = "")


# Save the plot as a PNG file
png(
  "sentiment_by_topic_dictionaries1.png",
  width = 5.5,
  height = 2.25,
  units = "in",
  res = 300
)
print(sd3)
dev.off()


###### NRC EMOTIONS

nrc <- top_topics_tokens %>%
  unnest(topic) |>
  inner_join(get_sentiments("nrc") %>%
               filter(!sentiment %in% c("positive",
                                        "negative"))) %>%
  count(topic, sentiment) |>
  left_join(topic_names, by = c("topic" = "id"))

emotion <-
  ggplot(nrc, aes(
    x = reorder(name, n),
    y = n,
    color = sentiment
  )) +
  geom_point(size = 3)+
  geom_line(aes(group = sentiment))+
  # geom_bar(stat = "identity",
  #          alpha = 2 / 3,
  #          show.legend = FALSE) +
  theme_minimal() +
  scale_color_manual(values = getPalette(colourCount)) +
  xlab("") + ylab("Number of words")+
  coord_flip()

# Save the plot as a PNG file
png(
  "emotion_by_topic.png",
  width = 7,
  height = 3.5,
  units = "in",
  res = 300
)
print(emotion)
dev.off()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# SENTIMENTS TOPIC TIME

sentiment_df <- top_topics_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  unnest(topic) |>
  mutate(
    date = as.Date(created_at),
    days_since_start = as.numeric(difftime(date, min(date), units = "days")),
    bin = floor(days_since_start / 7)
  ) |>
  group_by(bin, topic) |>
  mutate(tot_week = sum(value)) |>
  ungroup() |>
  left_join(topic_names, by = c("topic" = "id")) |> 
  select(bin, date, tot_week, name) |> 
  distinct()

# s_w_t <-
ggplot(sentiment_df, aes(x = bin, y = tot_week, fill = name)) +
  geom_bar(stat = "identity", alpha = 2 / 3, show.legend = FALSE) +
  scale_fill_manual(values = getPalette(colourCount)) +
  labs(x = "", y = "AFINN sentiment score") +
  theme_minimal() +
  facet_wrap( ~ name, ncol = 2)

s_w_t <-
  ggplot(sentiment_df, aes(x = bin, y = tot_week, color = name)) +
  geom_point(alpha = 1) +
  geom_line(show.legend = FALSE, aes(group = name), alpha = 0.8)+
  scale_color_manual(values = getPalette(colourCount), name = "Topic") +
  labs(x = "Week of data collection", y = "AFINN sentiment score") +
  theme_minimal() 

# Save the plot as a PNG file
png(
  "sentiment_by_week_topic2.png",
  width = 6,
  height = 4,
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
# TIME REGRESSION

s_df <- sentiment_df |> select(-date) |> distinct()
m <- lm(tot_week~bin+name, s_df)
summary(m)
