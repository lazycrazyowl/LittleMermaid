# word frequency

library(tidyverse)
library(tidytext)
library(udpipe)
library(lubridate)
library(wordcloud)
library(wordcloud2)
library(sentimentr)

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
#ud_english <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")

# 
#tagged <- udpipe(df$title, object = ud_english)
# 
#write_csv(tagged,"review_titles_tagged.csv")

tagged <- read_csv("review_titles_tagged.csv")

# adjectives
tagged %>%
  filter(upos == "ADJ") %>%
  count(lemma) %>%
  arrange(desc(n))

# adj + noun
df_bigram <- df %>% 
  unnest_tokens(bigram,title,token = "ngrams",n=2) %>%
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

df_adj_noun <- df_adj_noun%>%
  mutate(bigram = paste(word1,word2)) %>%
  count(bigram) %>%
  arrange(desc(n))

sentiments <- df_adj_noun$bigram %>%
  get_sentences() %>%
  sentiment()

df_adj_noun <- df_adj_noun %>%
  add_column(sentiment = sentiments$sentiment) 


df_adj_noun <- df_adj_noun %>% 
  mutate(sentiment = ifelse(str_detect(bigram,"iconic|emblematic"),
                             .5,sentiment)) %>%
  mutate(sentiment = ifelse(str_detect(bigram,"overrated|underwhelming"),
                            -.5,sentiment))

#df_adj_noun <- df_adj_noun %>%
#  mutate(n = if_else(sentiment < 0.0,n+1,n))

df_adj_noun$colours <- "grey"
df_adj_noun$colours <- case_when(df_adj_noun$sentiment < 0.0 ~ "darkred",
                                 df_adj_noun$sentiment == 0 ~ "grey",
                                 df_adj_noun$sentiment > 0.0 ~ "steelblue")


words = df_adj_noun$bigram
weights = df_adj_noun$n
colorlist = df_adj_noun$colours


wordcloud(words,freq = weights,
          scale=c(6, 1), random.order = FALSE, 
          random.color = FALSE, colors=colorlist,ordered.colors=TRUE,
          min.freq =2,rot.per=0.1)

ggsave("wordcloud_mermaid.png",width=13,height=13)

# wordcloud2(df_adj_noun,
#            color = df_adj_noun$colours,
#            rotateRatio=1,
#            size = 1)



ggplot(df_adj_noun, aes(label = bigram, size = n,colour=colours),min.f) +
  #geom_text_wordcloud() +
  theme_minimal() +
  geom_text_wordcloud_area(aes(angle = 45 * sample(-2:2, nrow(df_adj_noun),
                                                   replace = TRUE,
                                                   prob = c(1, 1, 4, 1, 1))))

ggplot(df_adj_noun,aes(label = bigram, size = n*10,color = sentiment)) +
         geom_text_wordcloud_area(aes(angle = sample(c(0,0,0,0,-15,15,seq(-75,75,15)),
                                                     nrow(df_adj_noun),replace=T))) +
  scale_size_area(max_size = 20) +
  #scale_radius(range = c(0, 20), limits = c(0, NA)) +
  theme_minimal()
                                      
  # )),
  # area_corr_power = 1,
  # mask = png::readPNG(system.file("extdata/hearth.png",
  #                                 package = "ggwordcloud", mustWork = TRUE
  # )),
  # rm_outside = TRUE
  # ) +
  # scale_size_area(max_size = 25) +
  # theme_minimal() +
  # scale_color_gradient(low = "darkred", high = "red")

# "a" + adj + noun
df_trigram <- df %>% 
  unnest_tokens(trigram,title,token = "ngrams",n=3) %>%
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

df_a_adj_noun <- df_a_adj_noun %>%
  mutate(trigram = paste(word1,word2,word3)) %>%
  count(trigram)

sentiments <- df_a_adj_noun$trigram %>%
  get_sentences() %>%
  sentiment()

df_a_adj_noun <- df_a_adj_noun %>%
  add_column(sentiment = sentiments$sentiment) 

df_a_adj_noun <- df_a_adj_noun %>%
  mutate(n = ifelse(sentiment<0.0,n*5,n))

df_a_adj_noun$colours <- case_when(df_a_adj_noun$sentiment < 0.0 ~ "darkred",
                                   df_a_adj_noun$sentiment == 0 ~ "grey",
                                   df_a_adj_noun$sentiment > 0 ~ "steelblue")

# wordcloud(df_a_adj_noun$trigram,df_a_adj_noun$n,
#            color = df_a_adj_noun$colours, ordered.colors = T,scale = c(2,.5),
#           min.freq = 5)

df_a_adj_noun2 <- df_a_adj_noun %>%
  filter(n>9) %>%
  mutate(trigram = str_remove(trigram, "a "))

wordcloud2(df_a_adj_noun2,
          color = df_a_adj_noun2$colours,
          rotateRatio=1,
          size = .5)

# a + adj + walk
df_a_adj_walk <- df_trigram %>%
  filter(word1 == "a",word2 %in% include2$token,
         word3 == "walk")

df_a_adj_walk <- df_a_adj_walk %>%
  mutate(trigram = paste(word1,word2,word3)) %>%
  count(trigram) 

sentiments <- df_a_adj_walk$trigram %>%
  get_sentences() %>%
  sentiment()

df_a_adj_walk <- df_a_adj_walk %>%
  add_column(sentiment = sentiments$sentiment) %>%
  arrange(desc(n))


