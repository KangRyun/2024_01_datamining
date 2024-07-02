### Library
library(caret)
library(dplyr)

### Library
library(e1071)

### 훈련자료: winequality_red_train.csv
data.taiin <- read.csv('./data/winequality_red_train.csv', header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header = T, stringsAsFactors = T)

### 모형 적합
fit.naive <- naiveBayes(quality ~ ., data=data.train)


### 검정자료 예측
pred.naive <- predict(fit.naive, data.test)

### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$quality, data=pred.naive, positive="Good")

### ROC curve & AUC
prob.naive <- predict(fit.naive, data.test, type="raw")
ROSE::roc.curve(data.test$quality, prob.naive[,"Good"])

### 중요도 계산
#install.packages("klaR")
library(klaR)

model <- train(quality ~ ., data=data.train, method="nb")
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)



