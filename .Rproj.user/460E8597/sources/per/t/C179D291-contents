### library
library(dplyr)
library(lattice)
library(udpipe)

### N(체언) 추출 함수
fn_extract_noun <- function(data, pos = 9) {
  data_split_lemma <- base::strsplit(data["lemma"], split = "+", fixed = T)[[1]]
  data_split_xpos <- base::strsplit(data["xpos"], split = "+", fixed = T)[[1]]
  retern_val <- NA
  if(length(data_split_lemma)==length(data_split_xpos)){
    idx_n <- substr(data_split_xpos, 1, 1)=="n"
    if(sum(idx_n)>0) retern_val <- data_split_lemma[idx_n]
  }
  retern_val
}

fn_extract_noun_only <- function(txt, rm.dup = F, pos = 9) {
  res <- udpipe(x = txt, object = "korean-kaist") %>% apply(1, fn_extract_noun, pos = pos) %>% unlist()
  res <- res[!is.na(res)]
  if(rm.dup) res <- unique(res)
  res
}

### 파일 읽기
song.100 <- read.csv('data/멜론_202404_100곡.csv', header = T, encoding = 'UTF-8')

### 가사 선택
song.text <- song.100$가사
song.text %>% class()
song.text %>% mode()
### 줄바꿈(\n) 제거
song.text <- gsub('\n', ' ', song.text)

### 명사 추출 > 노래별 가사 중복 제거
text.noun <- sapply(song.text, fn_extract_noun_only, rm.dup = T)
names(text.noun) <- NULL
text.noun %>% head()

### 노래별 추출된 단어를 하나의 벡터로 변환
text.noun.all <- text.noun %>% unlist()
text.noun.all %>% head(50)

### 한글 추출
text.noun.han <- gsub('[^가-힣]', '', text.noun.all)
text.noun.han %>% head(50)

### 2자 이상 단어를 추출
text.noun.han <- text.noun.han[nchar(text.noun.han) > 1]
df_noun <- table(text.noun.han) %>% data.frame() %>% arrange(Freq %>% desc())
df_noun %>% head(20)

### library
library(ggplot2)
### 막대 그래프
ggp <- df_noun %>% head(20) %>%
  ggplot(aes(text.noun.han %>% reorder(-Freq), Freq)) +
  geom_col(fill = "skyblue") +
  xlab("단어") +
  theme_minimal()
ggp

### library
library(wordcloud2)
df_noun %>% wordcloud2(minSize = 10)
df_noun %>%
  wordcloud2(
    fontFamily='NanumGothic',
    size = 0.8, color = 'random-dark',
    minRotation=0, maxRotation=0,
    minSize=10)

### Library
library(tidyverse)
library(tidygraph)
library(ggraph)

data.df <- data.frame(keyword = text.noun.han)

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

bigram.df %>% as_tbl_graph() %>% plot()

bigram.df %>% as_tbl_graph() %>% 
  ggraph(layout = 'nicely') +
  geom_edge_link() +
  geom_node_text(aes(label=name))

### News
data.raw <- readLines('data/news_keyword.txt')
data.raw <- data.raw[-1]
data.raw %>% head()

data.split <- strsplit(data.raw, ',')
data.splitdata.split
