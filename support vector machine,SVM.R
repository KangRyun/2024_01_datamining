library(caret)
library(dplyr)
### 훈련자료: winequality_red_train.csv
data.tarin <- read.csv('./data/winequality_red_train.csv', header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header = T, stringsAsFactors = T)

### Library
library(e1071)

### 모형적합
fit.svm <- svm(quality ~ ., data=data.train,
               type='C-classification', kernel='radial',
               probability=TRUE,
               gamma=0.1, cost=1)

### 정오분류표
### 검정자료 예측
pred.svm <- predict(fit.svm, data.test)
### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$quality, data=pred.svm, positive='Good')


### ROC curve & AUC
pred.svm <- predict(fit.svm, data.test, probability = T)
prob.svm <- pred.svm %>% attr('probabilities')
ROSE::roc.curve(data.test$quality, prob.svm[,'Good'], main="ROC curve - SVM")

### Library
#install.packages("kernlab")
library(kernlab)
### 중요도 계산
model <- train(quality ~ ., data=data.train, method="svmRadial")
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)
