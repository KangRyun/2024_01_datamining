### Library
library(caret)
library(dplyr)

### 훈련자료
data.train.raw <- read.csv('./data/heart_train.csv', header = T, stringsAsFactors = T)

### 검정자료
data.test.raw <- read.csv('./data/heart_test.csv', header = T, stringsAsFactors = T)

### 데이터 처리
data.train.raw %>% sapply(class)

data.train <- data.train.raw
data.train %>% sapply(class)

data.train$output <- c('less', 'more')[data.train$output+1] %>% factor()
data.train %>% head()

data.train$cp %>% table()
data.train$cp <- c('Value_1', 'Value_2', 'Value_3', 'Value_4')[data.train$cp+1] %>% factor()
data.train$cp


data.train$restecg <- c('Value_0', 'Value_1', 'Value_2')[data.train$restecg+1] %>% factor()

### 데이터 처리
# test
data.test.raw %>%  sapply(class)

data.test <- data.test.raw %>% 
  mutate(cp = paste('Value_', cp+1, sep='') %>% factor(),
         restecg = paste('Value_', restecg, sep='') %>% factor(),
         output = paste(c('less', 'more')[output+1]) %>% factor())

data.test


### 교차표
table(data.test$cp, data.test$output)
table(data.test$caa, data.test$output)
tapply(data.test$oldpeak, data.test$output, mean)
