### Library
library(caret)
library(dplyr)

### 훈련자료: winequality_red_wine_train.csv
data.train <- read.csv('./data/winequality_red_train.csv', header=T, stringsAsFactors=T)
### 검정자료: winequality_red_wine_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header=T, stringsAsFactors=T)

# > CART: Classfication and Regression Trees
### Library
library(rpart)
library(rpart.plot)

### 모형적합
fit.rpart <- rpart(quality ~ ., data=data.train)
fit.rpart %>% summary()
### 모형확인
prp(fit.rpart, type = 2, extra = 4)

rpart.plot(fit.rpart)

### 검정자료 예측
pred.rpart <- predict(fit.rpart, data.test, type='class')

df.pred <- cbind(data.test, pred.rpart)

### 정오분류표
#table(data.test$quality, pred.rpart)
table(pred.rpart, data.test$quality)


### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$quality, data=pred.rpart, positive='Good')


### ROC curve & AUC
prob.rpart <- predict(fit.rpart, data.test, type='prob')
ROSE::roc.curve(data.test$quality, prob.rpart[,'Good'])

### 변수 중요도
fit.rpart$variable.importance
model <- train(quality ~ ., data=data.train, method='rpart')
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)

# > C4.5
library(RWeka)
library(partykit)

### 모형적합
fit.c4.5 <- J48(quality ~ ., data = data.train)
### 모형확인
plot(fit.c4.5)

### 검정자료 예측
pred.c4.5 <- predict(fit.c4.5, data.test)
### 정오분료표
#table(data.test$quality, pred.c4.5)
### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$quality, data=pred.c4.5, positive='Good')

### ROC curve & AUC
ROSE::roc.curve(data.test$quality, prob.rpart[,'Good'])

prob.c4.5 <- predict(fit.c4.5, data.test, type='prob')
ROSE::roc.curve(data.test$quality, prob.c4.5[,'Good'], add.roc=T, col='red', lty=2, lwd=2)
