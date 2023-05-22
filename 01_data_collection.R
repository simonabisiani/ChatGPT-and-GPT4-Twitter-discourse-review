# DATA COLLECTION SCRIPT

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries
# install.packages("tidyverse")
# devtools::install_github("cjbarrie/academictwitteR", build_vignettes = TRUE)
library(tidyverse)
library(academictwitteR)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CHATGPT COLLECTION

# query formulation
and <- "chatgpt"
or <- c("employment",
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
        "economy")

queries <- paste(and, or)

bearer_token <- "RESEARCHER_SECRET_TOKEN"

chatgpt_collection <- data.frame()
for (i in queries) {
  iteration_i <- get_all_tweets(
    query = i,
    start_tweets = "2023-11-30T00:00:00Z",
    end_tweets = "2023-03-27T00:00:00Z",
    n = Inf,
    bearer_token = bearer_token,
    data_path = "chatgpt_march_collection/")
  if(nrow(iteration_i) > 0) {
    iteration_i$query <- i
    chatgpt_march_collection <- bind_rows(chatgpt_march_collection, iteration_i)
  }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GPT4 DATA COLLECTION

and <- "GPT4"
or <- c("employment",
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
        "economy")

queries <- paste(and, or)

gpt4_collection <- data.frame()
for (i in queries) {
  iteration_i <- get_all_tweets(
    query = i,
    start_tweets = "2022-11-30T00:00:00Z",
    end_tweets = "2023-03-27T00:00:00Z",
    n = Inf,
    bearer_token = bearer_token,
    data_path = "gpt4_collection/")
  if(nrow(iteration_i) > 0) {
    iteration_i$query <- i
    gpt4_collection <- bind_rows(gpt4_collection, iteration_i)
  }
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MERGE

full_dataset <- bind_rows(chatgpt_collection, gpt4_collection, .id = 'dataset')
# saveRDS(full_dataset, "full_dataset.RDS")
