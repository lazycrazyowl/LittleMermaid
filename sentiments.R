
library(sentimentr)
library(tidyverse)
library(lubridate)

df <- read_csv("littleMermaid_5000reviews_2019-08-06.csv") %>%
  mutate(element_id = as.character(row_number()),
         date = lubridate::dmy(date),
         quarter_date = floor_date(date,"quarter"),
         quarter = zoo::as.yearqtr(quarter_date))

sentiment_score <- df$review %>%
  get_sentences() %>%
  sentiment() %>%
  mutate(element_id = as.character(element_id)) %>%
  group_by(element_id) %>%
  summarise(sentiment = mean(sentiment))

df <- df %>% inner_join(sentiment_score)

df %>%
  group_by(quarter) %>%
  summarise(sentiment = mean(sentiment)) %>%
  ggplot(aes(factor(quarter),sentiment,group=1)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle=45))
