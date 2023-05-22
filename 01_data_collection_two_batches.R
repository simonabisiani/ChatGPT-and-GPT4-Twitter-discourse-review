# install.packages("tidyverse")
# devtools::install_github("cjbarrie/academictwitteR", build_vignettes = TRUE)
library(tidyverse)
library(academictwitteR)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# FIRST BATCH

# On 24th February we collected, using the Twitter API and the R package academictwitteR, any tweets containing our query between 30th November 2022 and 24th February 2023. The data collection was only possible through an Academic Twitter API developer account, due to: the number of tweets wanted, and the possibility to search the full archive. With a standard developer account (e.g. Elevated), this would have not been possible.

chatgpt_full_collection <- data.frame()
and <- "chatgpt"
or <- c(
  "employment",
  "employability",
  "employer",
  "employee",
  "unemployment",
  "unemployed",
  "job",
  "work",
  "skill",
  "taking over",
  "replace human",
  "performance",
  "concern",
  "insecurity",
  "fear",
  "threat",
  "opportunities",
  "training",
  "creation",
  "displacement",
  "occupation",
  "earning",
  "future demand",
  "education",
  "labour",
  "labor",
  "task",
  "industry",
  "workforce",
  "shortage",
  "collaboration",
  "collaborate",
  "human ai team",
  "team",
  "economic",
  "economy"
)

queries <- paste(and, or)

for (i in queries) {
  iteration_i <- get_all_tweets(
    query = i,
    start_tweets = "2022-11-30T00:00:00Z",
    end_tweets = "2023-02-24T00:00:00Z",
    n = Inf,
    bearer_token = bearer_token,
    # NOTE! the bearer token is not included in this notebook, making this code chunk impossible to run unless in possession of a Twitter API Academic account
    data_path = "chatgpt_full_collection/"
  )
  iteration_i$query <- i
  chatgpt_full_collection <-
    bind_rows(chatgpt_full_collection, iteration_i)
}

saveRDS(chatgpt_full_collection, "chatgpt_full_collection.RDS")

# data <- readRDS("chatgpt_full_collection.RDS")
# tweets <- data %>% select(text, query, created_at, author_id)
# file <- toJSON(tweets)
# write(file, "full_dataset_chatgpt.json")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TESTING PLURAL/SINGULAR QUERIES

# DO PLURAL WORDS CAPTURE SINGULAR ONES (or viceversa)
# There is little knowledge on query formulation on Twitter. Our assumption was that the singular form of the word (or its canonical form) would return tweets containing the plural form too. However, using another function from the academictwitteR package, which allows us to check for the number of tweets resulting from a query without needing to download the data, we found out this appears not to be true.

# We ran the code below to obtain the number of tweets for each of the singular/plural pairs, and saved the data. Let's load the data in the notebook so we can visualise the differences in counts between singular/plural queries.

# and <- "chatgpt"
# or <- c("employee",
#         "employees", # post collection insertion
#         "job",
#         "jobs", # post collection insertion
#         "skill",
#         "skills", # post collection insertion
#         "concern",
#         "concerns", # post collection insertion
#         "threat",
#         "threats", # post collection insertion
#         "occupation", # captures occupations
#         "occupations", # post collection insertion
#         "earning", # captures earnings
#         "earnings", # post collection insertion
#         "task", # captures tasks
#         "tasks", # post collection insertion
#         "team", # NEW SUGGESTION (would also capture the above)
#         "teams") # post collection insertion
#
# queries <- paste(and, or)
#
# query_counts <- data.frame()
# for (i in queries) {
#   iteration_i <- count_all_tweets(i,
#                                   "2022-11-30T00:00:00Z",
#                                   "2023-02-24T00:00:00Z",
#                                   bearer_token,
#                                   n = 500)
#   iteration_i$query <- i  # Create a new column "query" and assign it the value of i
#   query_counts <- bind_rows(query_counts, iteration_i)}
#
# saveRDS(query_counts, "query_counts.RDS")

# Let's now see what we can figure out from comparing singular/plural versions of the same word.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Following the analysis above, we can tell that there isn't a clear overlap in the number of tweets returned when searching for singular or plural. Furthermore, while at times searching for a singular word returns more results than the plural equivalent, in other instances the opposite is true.
# Thus we decided to run a second data collection for the plural form of the words, using the code below.

and <- "chatgpt"
or <- c(
  "employees",
  "jobs",
  "skills",
  "concerns",
  "threats",
  "occupations",
  "earnings",
  "tasks",
  "collaborations"
) 

queries <- paste(and, or)

chatgpt_second_collection <- data.frame()
for (i in queries) {
  iteration_i <- get_all_tweets(
    query = i,
    start_tweets = "2022-11-30T00:00:00Z",
    end_tweets = "2023-02-24T00:00:00Z",
    n = Inf,
    bearer_token = bearer_token,
    data_path = "chatgpt_second_collection/"
  )
  iteration_i$query <-
    i  # Create a new column "query" and assign it the value of i
  chatgpt_second_collection <-
    bind_rows(chatgpt_second_collection, iteration_i)
}

saveRDS(chatgpt_second_collection, "chatgpt_second_collection.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECOND BATCH

# In March, we repeated the data collection as we realised we had more Twitter API allowance, and also extended the collection to GPT4.

# Chatgpt query
and <- "chatgpt"
or <- c(
  "employment",
  "employability",
  "employer",
  "employees",
  "employee",
  "unemployment",
  "unemployed",
  "job",
  "jobs",
  "work",
  "skill",
  "skills",
  "taking over",
  "replace human",
  "performance",
  "concern",
  "concerns",
  "insecurity",
  "fear",
  "threat",
  "threats",
  "opportunities",
  "training",
  "creation",
  "displacement",
  "occupation",
  "occupations",
  "earning",
  "earnings",
  "future demand",
  "education",
  "labour",
  "labor",
  "task",
  "tasks",
  "industry",
  "workforce",
  "shortage",
  "collaboration",
  "collaborations",
  "collaborate",
  "human ai team",
  "team",
  "teams",
  "economic",
  "economy"
)

queries <- paste(and, or)

# ChatGPT second collection
bearer_token <- "RESEARCHER_SECRET_TOKEN"

chatgpt_march_collection <- data.frame()
for (i in queries) {
  iteration_i <- get_all_tweets(
    query = i,
    start_tweets = "2023-02-25T00:00:00Z",
    end_tweets = "2023-03-27T00:00:00Z",
    n = Inf,
    bearer_token = bearer_token,
    data_path = "chatgpt_march_collection/"
  )
  if (nrow(iteration_i) > 0) {
    iteration_i$query <- i
    chatgpt_march_collection <-
      bind_rows(chatgpt_march_collection, iteration_i)
  }
}

saveRDS(chatgpt_march_collection, "chatgpt_march_collection.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GPT4 query
and <- "GPT4"
queries <- paste(and, or)

gpt4_collection <- data.frame()
for (i in queries) {
  iteration_i <- get_all_tweets(
    query = i,
    start_tweets = "2022-11-30T00:00:00Z",
    end_tweets = "2023-03-27T00:00:00Z",
    n = Inf,
    bearer_token = bearer_token,
    data_path = "gpt4_collection/"
  )
  if (nrow(iteration_i) > 0) {
    iteration_i$query <- i
    gpt4_collection <- bind_rows(gpt4_collection, iteration_i)
  }
}

saveRDS(gpt4_collection, "gpt4_collection.RDS")


full_dataset <- bind_rows(gpt4_collection, chatgpt_march_collection, chatgpt_second_collection, chatgpt_full_collection, .id = 'dataset')
# saveRDS(full_dataset, "full_dataset.RDS")
full_dataset <- readRDS("full_dataset.RDS")