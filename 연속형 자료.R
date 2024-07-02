library(caret)
library(dplyr)

### 훈련자료: winequality_red_wine_train.csv
data.train <- read.csv('./data/mtcars_train.csv', header=T, stringsAsFactors=T)
### 검정자료: winequality_red_wine_test.csv
data.test <- read.csv('./data/mtcars_test.csv', header=T, stringsAsFactors=T)

###선형회귀
fit.lm <- lm(mpg ~., data = data.train)

fit.lm %>% summary()

### 검정자료 예측
pred.lm <- predict(fit.lm, data.test)

### 실제 값
true.values <- data.test$mpg

### 예측 값
pred.values <- pred.lm

### 오차
error.value <- true.values - pred.values
MSE <- mean(error.value^2)

### RMSE
RMSE <- sqrt(MSE); RMSE

plot(true.values, pred.values, pch = 19)
abline(0, 1, col = "gray", lty = 2)

### 변수 선택
fit.lm.step <- step(fit.lm)
fit.lm.step %>% summary()

#######################################################

### 의사결정나무 (CART)
library(rpart)
library(rpart.plot)

### 모형 적합
fit.rpart <- rpart(mpg ~., data = data.train)

### 모형 확인
prp(fit.rpart)

### 검정자료 예측
pred.rpart <- predict(fit.rpart, data.test)

RMSE(data.test$mpg, pred.rpart)

#######################################################
###  S V M
library(e1071)

### 모형 적합
fit.svm <- svm(mpg ~., data = data.train, type = "eps-regression", probability = TRUE, cost = 100)

### 검정자료 예측
pred.svm <- predict(fit.svm, data.test)

RMSE(data.test$mpg, pred.svm)

### 변수 중요도
library(kernlab)

### 중요도 계산
model <- train(mpg ~., data = data.train)
importance <- varImp(model, scale = FALSE)
plot(importance, cex.lab = 0.5)

### pRoc 통계적 검정을 해주는 packages
### install.packages("pROC")
library(pROC)

### 신경망
## nnet
## neural network
library(nnet)
library(NeuralNetTools)
# 다음 시간에....


### 랜포
library(randomForest)

### 모형 적합
fit.rf <- randomForest(mpg ~ ., data=data.train, ntree = 100, importance=T)

### 검정자료 예측
pred.rf <- predict(fit.rf, data.test)

RMSE(data.test$mpg, pred.rf)

### 중요도 계산
importance(fit.rf)

### 중요도 plot
varImpPlot(fit.rf, main='importance')
