library(dplyr)

data.file <- 'data/news_keyword.txt'
data.raw <- readLines(data.file)
data.raw <- data.raw[-1]
data.raw %>% head()

data.split <- strsplit(data.raw, ",")
data.split <- sapply(data.split, unique)
data.split %>% head(1)

data.vector <- data.split %>% unlist()
data.vector %>% head(250)

### 단어 빈도
df_noun <- table(data.vector) %>% 
  data.frame() %>% 
  arrange(Freq %>% desc())
df_noun %>% head()


### library
library(ggplot2)
### 막대 그래프
ggp <- df_noun %>% head(20) %>%
  ggplot(aes(data.vector %>% reorder(-Freq), Freq)) +
  geom_col(fill = "skyblue") +
  xlab("단어") +
  theme_minimal() +
  coord_flip()
ggp

### library
library(wordcloud2)

df_noun %>% 
  head(20) %>% 
  wordcloud2(
    fontFamily='NanumGothic',
    size = 0.8, color = 'random-dark',
    minRotation=0, maxRotation=0,
    minSize=10)

### Library
library(tidyverse)
library(tidygraph)
library(ggraph)

data.df <- data.frame(keyword = data.vector)
bigram.df <- 
  data.df %>%
  mutate(lead=lead(keyword)) %>%
  filter(keyword != lead) %>% 
  unite(bigram, c(keyword, lead), sep=' ') %>% 
  count(bigram, sort=TRUE) %>% 
  filter(n>=2) %>% 
  separate(bigram, c('word1', 'word2'), sep=' ')
bigram.df %>% dim()
bigram.df %>% head()

### 그림 2
bigram.df %>% head(50) %>% as_tbl_graph() %>%
  ggraph(layout='nicely') +
  geom_edge_link() +
  geom_node_text(aes(label=name))
