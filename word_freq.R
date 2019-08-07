# word frequency

library(tidyverse)
library(tidytext)
library(udpipe)
library(lubridate)

df <- read_csv("littleMermaid_5000reviews_2019-08-06.csv")

# top ten content words
df %>% 
  unnest_tokens(word,review,token = "words") %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("copenhagen","mermaid","statue")) %>%
  #filter(rating==3) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(10) 

# top ten title words
df %>% 
  unnest_tokens(word,title,token = "words") %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("copenhagen","mermaid","statue")) %>%
  filter(rating==5) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(10)

# bigrams
df %>% 
  unnest_tokens(bigram,review,token = "ngrams",n=2) %>%
  count(bigram) %>% 
  arrange(desc(n))

# trigrams
df %>% 
  unnest_tokens(trigram,review,token = "ngrams",n=3) %>%
  #eparate(trigram,c("word1","word2","word3")) %>%
  count(trigram) %>% 
  arrange(desc(n))

# PoS frequencies

#language_nodel <- udpipe_download_model(language = "english-ewt")
ud_english <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")
# 
# tagged <- udpipe(df$review, object = ud_english)
# 
# write_csv(tagged,"reviews_tagged.csv")

tagged <- read_csv("reviews_tagged.csv")

# adjectives
tagged %>%
  filter(upos == "ADJ") %>%
  count(lemma) %>%
  arrange(desc(n))

# adj + noun
df_bigram <- df %>% 
  unnest_tokens(bigram,review,token = "ngrams",n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")
  
include1 <- udpipe(x = df_bigram$word1,
                   object = ud_english)

include2 <- udpipe(x = df_bigram$word2,
                  object = ud_english)

include1 <- include1      %>%
  select(token,upos)    %>%
  filter(upos =="ADJ") %>%
  select(token) 

include2 <- include2      %>%
  select(token,upos)    %>%
  filter(upos =="NOUN") %>%
  select(token) 

df_adj_noun <- df_bigram %>%
  filter(word1 %in% include1$token,
         word2 %in% include2$token)

df_adj_noun %>%
  mutate(bigram = paste(word1,word2)) %>%
  count(bigram) %>%
  arrange(desc(n))

# "a" + adj + noun
df_trigram <- df %>% 
  unnest_tokens(trigram,review,token = "ngrams",n=3) %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")
  #mutate(trigram = paste(word1,word2,word3)) %>%
  #count(trigram) %>%
  #arrange(desc(n))

include2 <- udpipe(x = df_trigram$word2,
                   object = ud_english)

include3 <- udpipe(x = df_trigram$word3,
                   object = ud_english)

include2 <- include2    %>%
  select(token,upos)    %>%
  filter(upos =="ADJ")  %>%
  select(token) 

include3 <- include3    %>%
  select(token,upos)    %>%
  filter(upos =="NOUN") %>%
  select(token) 

df_a_adj_noun <- df_trigram %>%
  filter(word1 == "a",word2 %in% include2$token,
         word3 %in% include3$token)

df_a_adj_noun %>%
  mutate(trigram = paste(word1,word2,word3)) %>%
  count(trigram) %>%
  arrange(desc(n))

# a + adj + walk
df_a_adj_walk <- df_trigram %>%
  filter(word1 == "a",word2 %in% include2$token,
         word3 == "walk")

df_a_adj_walk %>%
  mutate(trigram = paste(word1,word2,word3)) %>%
  count(trigram) %>%
  arrange(desc(n))
