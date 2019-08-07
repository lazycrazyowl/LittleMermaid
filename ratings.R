# ratings

library(tidyverse)
library(tidytext)
library(udpipe)
library(lubridate)

df <- read_csv("littleMermaid_5000reviews_2019-08-06.csv")

df <- df %>%
  mutate(date = lubridate::dmy(date),
         quarter_date = floor_date(date,"quarter"),
         quarter = zoo::as.yearqtr(quarter_date))

# reviews cumulated
df %>%
  group_by(month=floor_date(date, "month")) %>%
  summarise(n = n()) %>%
  mutate(cumsum = cumsum(n)) %>%
  ggplot(aes(x=month,y=cumsum)) +
  geom_line(size=1) +
  geom_point(size=2) +
  theme_minimal() +
  labs(y="Antal anmeldelser akk.",x="Måned") +
  theme(axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_text(margin = margin(r = 20,l=10))) 

########## 5. Distribution of ratings #####################

df %>% 
  group_by(rating) %>%
  count() %>%
  ggplot(aes(x=factor(rating),y=n,fill=factor(rating))) + 
  geom_col() +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"),guide=F) +
  labs(y="Antal",x="Rating") +
  theme_minimal() +
  theme(axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_text(margin = margin(r = 20,l=10))) 

# reviews by calendar month
df %>% group_by(month(date)) %>% count() %>%
  ggplot(aes(factor(`month(date)`),n)) + geom_col()

# reviews by quarter
df %>%
  group_by(quarter) %>%
  count() %>%
  ggplot(aes(factor(quarter),n)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45))

# mean rating by month
df %>%
  group_by(mnth = floor_date(date,"month")) %>%
  summarise(m_rating = mean(rating)) %>%
  ggplot(aes(mnth,m_rating)) +
  geom_line() +
  geom_smooth()

# rating distribution by quarter

df %>%
  group_by(quarter,rating) %>%
  count() %>%
  group_by(quarter) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(factor(quarter),pct,fill=rating)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=45))





