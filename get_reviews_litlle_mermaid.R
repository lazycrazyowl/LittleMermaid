devtools::install_github("PeerCHristensen/tripR")

library(tripR)
library(tidyverse)

base_url <- "https://www.tripadvisor.co.uk/Attraction_Review-g189541-d245024-Reviews-The_Little_Mermaid_Den_Lille_Havfrue-Copenhagen_Zealand.html"

df <- get_reviews(base_url,
                  page_lim = 1000) # 5000 reviews

write_csv(df,"litlleMermaid_reviews.csv")

