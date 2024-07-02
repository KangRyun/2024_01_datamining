### library
library(dplyr)
library(lattice)
library(udpipe)

### 파일 읽기
data.raw <- read.csv('data/Hotel_Reviews.csv', header = T)

data.raw %>% dim()
data.raw %>% names()
data.raw %>% str()

### 수집된 호텔의 수
data.raw %>% select(Hotel_Name) %>% unique() %>% count()


### 리뷰가 많은 호텔은 어디?
tb <- table(data.raw$Hotel_Name) %>% as.data.frame()
tb %>% arrange(Freq %>% desc()) %>% head(6)


data.use <- data.raw %>% filter(Hotel_Name == 'Britannia International Hotel Canary Wharf')
data.use %>% dim()


reviews.pos <- data.use$Positive_Review %>%  tolower()
reviews.tar <- data.use$Positive_Review %>%  tolower()

x <- udpipe(x = reviews.tar, object = 'english') %>% as.data.frame()
x %>% head()

stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

### NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring adjectives", xlab = "Freq")


## Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

## Using Pointwise Mutual Information Collocations
x$word <- tolower(x$token)
stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")

## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")


cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))


library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  #theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")

