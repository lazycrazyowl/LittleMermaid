# biterm topic modelling

library(BTM)
library(udpipe)
library(tidyverse)
library(magrittr)
library(tidytext)
library(tm)

tagged <- read_csv("reviews_tagged.csv")

tagged %<>%
  filter(!lemma %in% c("copenhagen","mermaid","statue","more","little")) %>%
  #filter(upos %in% c("NOUN")) %>%
  filter(xpos %in% c("NN", "NNP", "NNS"))%>% # NNP = Prop. nouns, elicits comparisons
  select(doc_id,lemma) 

## Building the model
set.seed(321)
model3  <- BTM(tagged, k = 3, beta = 0.01, iter = 500, trace = 100)
model5  <- BTM(tagged, k = 5, beta = 0.01, iter = 500, trace = 100)
model7a  <- BTM(tagged, k = 7, beta = 0.01, iter = 1000, trace = 100,background=T)



## Inspect the model - topic frequency + conditional term probabilities
model3$theta
model5$theta
model7a$theta


topicterms3 <- terms(model3, top_n = 5)
topicterms3

topicterms5 <- terms(model5, top_n = 5)
topicterms5

topicterms7a <- terms(model7a, top_n = 5)
topicterms7a

