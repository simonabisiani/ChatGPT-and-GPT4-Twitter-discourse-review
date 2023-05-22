# Compare top terms for LDA with k = c(12,14,20) for the four datasets

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries
library(furrr)
library(tidyverse)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run LDA on the four document-term-matrices

# List of document term matrices from previous script
dtm_list <- list(dtm1, dtm2, dtm3, dtm4)
names(dtm_list) <- c("dtm1", "dtm2", "dtm3", "dtm4")

# Function to run LDA for a single dtm and a list of K values
plan(multisession)
run_lda_for_dtm <- function(dtm, k_values) {
  tibble(K = k_values) %>%
    mutate(topic_model = future_map(K, ~ LDA(
      dtm,
      k = .,
      control = list(seed = 1234),
      method = "Gibbs"
    )), seed = TRUE)
}

# Run models for each document term matrix and each value of K
k_values <- c(12, 14, 20)
# many_models <- lapply(dtm_list, run_lda_for_dtm, k_values) # this takes a long time

# saveRDS(many_models, "many_models.RDS") # save object
many_models <- readRDS("many_models.RDS") # load object

# Combine results into a single data frame
df <- bind_rows(many_models, .id = "dataset")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Get top terms

# Function to get top terms for a given topic model
get_top_terms <- function(topic_model) {
  tidy(topic_model, matrix = "beta") %>%
    arrange(topic, desc(beta)) %>%
    group_by(topic) |>
    slice_max(order_by = beta, n = 15) %>%
    select(topic, term) %>%
    mutate(term = as.character(term))
}

# Extract LDA models from tibbles
lda_models_list <- lapply(many_models, function(dtm_models) {
  lapply(dtm_models$topic_model, function(model)
    model)
})

# Extract top terms from each individual model and bind them together at the end
top_terms_list <- lapply(lda_models_list, function(dtm_models) {
  lapply(dtm_models, get_top_terms)
})

# Combine the top terms for all models
combined_top_terms <- lapply(seq_along(dtm_list), function(idx) {
  dtm_name <- names(dtm_list)[idx]
  lapply(seq_along(k_values), function(idx2) {
    top_terms <- top_terms_list[[idx]][[idx2]]
    top_terms$dataset <- dtm_name
    top_terms$K <- k_values[idx2]
    top_terms
  })
})


# Flatten the combined list into a single data frame
final_top_terms <-
  do.call(rbind, unlist(combined_top_terms, recursive = FALSE)) |>
  pivot_wider(
    id_cols = c(dataset, K),
    names_from = topic,
    values_from = term,
    values_fn = list
  ) |>
  unnest(cols = 3:22) |>
  arrange(K, dataset)

# write to a google sheet for qualitative comparison
library(googlesheets4)
write_sheet(final_top_terms, ss = "https://docs.google.com/spreadsheets/d/1650HIlq1o1z4zMl9GvlGxmPineL3y44xOOeX_Z-I3wE/edit#gid=251884392", sheet = "top_terms_from_all_dfs_and_k_values")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# # Extract coherence and exclusivity
# library(topicdoc)
# 
# k_result <- df %>%
#   mutate(
#     exclusivity = map(topic_model, topic_exclusivity, top_n_tokens = 10),
#     coherence = map(
#       topic_model,
#       topic_coherence,
#       dtm,
#       top_n_tokens = 10,
#       smoothing_beta = 1
#     )
#   )
# 
# 
# # Preallocate columns for exclusivity and coherence
# df$exclusivity <- vector("list", nrow(df))
# df$coherence <- vector("list", nrow(df))
# 
# # Loop through each row of the data frame and calculate exclusivity and coherence
# for (i in seq_len(nrow(df))) {
#   df$exclusivity[[i]] <-
#     topic_exclusivity(df$topic_model[[i]], top_n_tokens = 10)
#   df$coherence[[i]] <-
#     topic_coherence(
#       df$topic_model[[i]],
#       df$dataset[[i]],
#       top_n_tokens = 10,
#       smoothing_beta = 1
#     )
# }
# 
# 
# # # visualise them side to side as line chart
# # p <- k_result %>%
# #   transmute(
# #     K,
# #     exclusivity = map_dbl(exclusivity, mean),
# #     coherence = map_dbl(coherence, mean)
# #   ) %>%
# #   gather(Metric, Value,-K) %>%
# #   ggplot(aes(K, Value)) +
# #   geom_line(size = 1,
# #             alpha = 0.9,
# #             show.legend = FALSE) +
# #   facet_wrap( ~ Metric, scales = "free_y") +
# #   labs(x = "K (number of topics)",
# #        y = NULL,
# #        title = "Model diagnostics by number of topics")
# #
# # # Save the plot as a PNG file
# # png(
# #   "topic_coherence_exclusivity.png",
# #   width = 8,
# #   height = 4,
# #   units = "in",
# #   res = 300
# # )
# # print(p)
# # graphics.off()
# #
# #
# # # not as helpful, here as a scatterplot
# # k_result %>%
# #   select(K, exclusivity, coherence) %>%
# #   filter(K %in% c(4, 10, 14, 20)) %>%
# #   unnest() %>%
# #   mutate(K = as.factor(K)) %>%
# #   ggplot(aes(coherence, exclusivity, color = K)) +
# #   geom_point(size = 2, alpha = 0.7) +
# #   labs(
# #     x = "Coherence",
# #     y = "Exclusivity",
# #     title = "Comparing exclusivity and coherence",
# #     subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity"
# #   )