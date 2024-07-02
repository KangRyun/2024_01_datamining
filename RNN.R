### Library
library(caret)
library(dplyr)
library(nnet)

### 훈련자료: winequality_red_train.csv
data.tarin <- read.csv('./data/winequality_red_train.csv', header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header = T, stringsAsFactors = T)

### 변수의 스케일 변환 - 표준화 변환
data.pre.pro <-  caret::preProcess(data.train, method = c("center", "scale"))
data.train <- predict(data.pre.pro, data.train)
data.test <- predict(data.pre.pro, data.test)

### 모형적합
fit.nnet <- nnet(quality ~ ., data=data.train, size=20, maxit=1000)

### plot
NeuralNetTools::plotnet(fit.nnet)

### 검정자료 예측### 검정자료 예측### 검정자료 예측
pred.nnet <- predict(fit.nnet, data.test, type='class') %>% factor()

### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$quality, data=pred.nnet, 
                       positive='Good', mode = 'everything')

### ROC curve & AUC
prob.nnet <- predict(fit.nnet, data.test, type='raw')
ROSE::roc.curve(data.test$quality, prob.nnet[,1])

### 중요도 계산
library(NeuralNetTools)
NeuralNetTools::garson(fit.nnet) + coord_flip()

NeuralNetTools::olden(fit.nnet) + coord_flip()

### Library: neuralnet {neuralnet}
library(neuralnet)

### 훈련자료: winequality_red_train.csv
data.tarin <- read.csv('./data/winequality_red_train.csv', header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header = T, stringsAsFactors = T)

### 변수의 스케일 변환 - 표준화 변환
data.pre.pro <-  caret::preProcess(data.train, method = c("center", "scale"))
data.train <- predict(data.pre.pro, data.train)
data.test <- predict(data.pre.pro, data.test)

### 모형적합
## stepmax <- 최대반복횟수
fit.mlp <- neuralnet(quality ~ ., data=data.train, 
                     hidden = c(4,2), stepmax = 1e+05,
                     linear.output = F, likelihood = T)

plot(fit.mlp)

fit.mlp$generalized.weights[[1]] %>% range()

gwplot(fit.mlp, selected.response = "Good", selected.covariate = "alchol", min = -100, max  = 100)
gwplot(fit.mlp, selected.response = "Good", selected.covariate = "volatile.acidity", min=-100, max=100)

comp.mlp <- compute(fit.mlp, data.test)
prob.mlp <- comp.mlp$net.result[,2]
pred.mlp <- ifelse(prob.mlp>0.5, "Good", "Bad") %>% factor()

caret::confusionMatrix(reference=data.test$quality, data=pred.mlp, positive = "Good", mode='everything')

중간고사 <- 데이터셋 제공 모형들로 모형훈련 평가 의사결정까지 시간 한시간 오픈북