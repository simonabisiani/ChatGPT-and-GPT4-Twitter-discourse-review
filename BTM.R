# BTM is for clustering short text (e.g. survey answers, twitter data, short sentences), LDA is for clustering long text (e.g. news articles, whole papers). BTM clusters word-word cooccurrences, LDA clusters word-document occurrences.

# BTM can be fit with R package BTM: https://cran.r-project.org/web//packages/BTM/index.html

# Here I try BTMs:
#install.packages("BTM")
#install.packages("udpipe")
library(BTM)
library(udpipe)
library(data.table)
library(stopwords)
annotations    <- udpipe(data1, "english", trace = 10)
biterms <- as.data.table(annotations)
biterms <- biterms[, cooccurrence(
  x = lemma,
  relevant = upos %in% c("NOUN", "ADJ", "VERB") &
    nchar(lemma) > 2 &
    !lemma %in% stopwords("en"),
  skipgram = 3
),
by = list(doc_id)]

set.seed(123456) # this was the seed for 20K and 15K
traindata <-
  subset(
    annotations,
    upos %in% c("NOUN", "ADJ", "VERB") &
      !lemma %in% stopwords("en") & nchar(lemma) > 2
  )

traindata <- traindata[, c("doc_id", "lemma")]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# One model, k = 14

model_btm     <-
  BTM(
    traindata,
    biterms = biterms,
    k = 14,
    iter = 1000
  )

library(textplot)
library(ggraph)
plot(model_btm, top_n = 15,
     title = "BTM model")

## Inspect the model - topic frequency + conditional term probabilities
model_btm$theta
topicterms <- terms(model_btm, top_n = 15)
topicterms1 <- do.call(cbind, topicterms)
write_csv(topicterms, "topicterms.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BTM COHERENCE 

# modified from tmca_coherence() function (by A. Niekler & G. Wiedemann): https://github.com/tm4ss/tm4ss.github.io
coherenceBTM <- function(model, DTM, N = 15) {
  
  # Ensure matrix or Matrix-format (convert if slam)
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(DTM)) {
    DTM <- sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  
  K <- model$K
  
  DTMBIN <- DTM > 0
  
  documentFrequency <- colSums(DTMBIN)
  names(documentFrequency) <- colnames(DTMBIN)
  
  topNtermsPerTopic <- terms(model, top_n = N)
  
  termcollect <- list()
  for (i in 1:K){
    termcollect[[i]] <- topNtermsPerTopic[[i]][,1]
  }
  
  allTopicModelTerms <- unique(as.vector(unlist(termcollect)))
  
  DTMBIN <- DTMBIN[, allTopicModelTerms]
  DTMBINCooc <- t(DTMBIN) %*% DTMBIN
  DTMBINCooc <- t((DTMBINCooc + 1) / colSums(DTMBIN))
  DTMBINCooc <- log(DTMBINCooc)
  DTMBINCooc <- as.matrix(DTMBINCooc)
  
  coherence <- rep(0, K)
  pb <- txtProgressBar(max = K)
  for (topicIdx in 1:K) {
    setTxtProgressBar(pb, topicIdx)
    topWordsOfTopic <- topNtermsPerTopic[[topicIdx]][,1]
    
    coherence[topicIdx] <- 0
    for (m in 2:length(topWordsOfTopic)) {
      for (l in 1:(m-1)) {
        mTerm <- as.character(topWordsOfTopic[m])
        lTerm <- as.character(topWordsOfTopic[l])
        coherence[topicIdx] <- coherence[topicIdx] + DTMBINCooc[mTerm, lTerm]
      }
    }
  }
  close(pb)
  
  return(coherence)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TOPIC MODEL LOOP OVER Ks

#The necessary Document-Term-Matrix (DTM) can be calculated with:

## DTM for coherence calculation ##
library(quanteda)
# one doc per row
x <- aggregate(traindata$lemma, by = list(traindata$doc_id), paste, collapse = " ")
names(x) <- c("doc_id", "lemma")
corpus <- corpus(x$lemma)
DFM <- dfm(tokens(corpus)) # edit parameters of tokens() to your needs, e. g. removal of separators and punctuation
DTM <- convert(DFM, to = "topicmodels")

# Define the range of K values you want to try
k_values <- seq(4,20,2)

# Initialize an empty list to store the coherence scores
coherence_scores_list <- list()

# Loop over the K values
for (k in k_values) {
  # Run the BTM model with the current K value
  btm_model <- BTM(traindata, biterms = biterms, k, iter = 1000)
  
  # Calculate the topic coherence for the model
  coherence_scores <- coherenceBTM(model = btm_model, DTM = DTM, N = 15)
  
  # Calculate the average coherence score for the current K value
  avg_coherence <- mean(coherence_scores)
  
  # Append the average coherence score to the list
  coherence_scores_list[[as.character(k)]] <- avg_coherence
}
