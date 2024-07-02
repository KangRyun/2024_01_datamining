### Library
library(caret)
library(dplyr)

library(randomForest)

### 훈련자료: winequality_red_wine_train.csv
data.train <- read.csv('./data/winequality_red_train.csv', header=T, stringsAsFactors=T)
### 검정자료: winequality_red_wine_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header=T, stringsAsFactors=T)

### 모형 적합
fit.rf <- randomForest(quality ~., data=data.train, ntree=200, importance=T)

### 검정자료 예측
pred.rf <- predict(fit.rf, data.test)

### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$quality, data = pred.rf,
                       positive='Good', mode = 'everything')

### ROC curve & AUC
prob.rf <- predict(fit.rf, data.test, type='prob')
ROSE::roc.curve(data.test$quality, prob.rf[,'Good'])

### 중요도 계산
importance(fit.rf)

### 중요eh plot
varImpPlot(fit.rf, main='importance')

