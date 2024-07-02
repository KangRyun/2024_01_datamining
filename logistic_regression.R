library(dplyr)
library(ROSE)
library(caret)

### 훈련자료: winequality_red_wine_train.csv
data.train <- read.csv(choose.files(), header=T, stringsAsFactors=T)
### 검정자료: winequality_red_wine_test.csv
data.test <- read.csv(choose.files(), header=T, stringsAsFactors=T)

data.train %>% head()
data.test %>% head

### 자료형 확인
data.train %>% sapply(class)

# 로지스틱
fit.glm <- glm(quality ~ ., data=data.train, family='binomial')
fit.glm %>% summary

### 검정자료 예측
pred.prob <- predict(fit.glm, newdata=data.test, type='response')
pred.prob %>% summary

pred.glm <- ifelse(pred.prob> 0.5, "Good","Bad") %>% factor()
pred.glm

### 정오분로표
table(data.test$quality, pred.glm)

### Cinfusion matrix and statistics
confusionMatrix(reference=data.test$quality, data=pred.glm, positive="Good")

### roc curve
roc.curve(data.test$quality, pred.glm)
roc.curve(data.test$quality, pred.prob)
