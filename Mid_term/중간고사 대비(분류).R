#### 중간고사 대비
### 분류, classification
### Data Load
library(dplyr)
library(ROSE)
library(caret)
library(randomForest)

df <- read.csv('./data/loan_data.csv', header=T, stringsAsFactors=T)
df %>% head()
df %>% sapply(class)


### pre-processing
# near zero var
data_nzv <- caret::nearZeroVar(df, saveMetrics = TRUE)
data_nzv

### data_nzv의 타입 확인
data_nzv %>% mode()
data_nzv

!data_nzv$zeroVar

data_use <- data_raw[,!data_nzv$zeroVar]
data_use %>% head()
data_use %>% sapply(class)


data_use <- df[!apply(df, 1, function(x) any(is.na(x) | x == "")), ]
data_use %>% head()

### label en
data_use$gender_encoded <- as.numeric(factor(data_use$Gender))
data_use$Married_encoded <- as.numeric(factor(data_use$Married))
data_use$Dependents_encoded <- as.numeric(factor(data_use$Dependents))
data_use$Education_encoded <- as.numeric(factor(data_use$Education))
data_use$Property_Area_encoded <- as.numeric(factor(data_use$Property_Area))
data_use$Self_Employed_encoded <- as.numeric(factor(data_use$Self_Employed))

data_use_1 <- data_use %>%
  select(where(~ !is.factor(.x)))

data_use_1$Loan_Status <- data_use$Loan_Status

### 훈련자료의 비율
train.ratio <- 0.7

### sampling 할때는 추출한 샘플을 파일로 저장 
### 훈련자료
set.seed(42)
train.index <- sample(1:nrow(data_use_1), nrow(data_use_1)*train.ratio)
data.train <- data_use_1[train.index,]
dim(data.train)

### 검정자료
data.test <- data_use_1[-train.index,]
dim(data.test)

### plot
data.train$Loan_Status %>% plot(x = )

# 로지스틱
fit.glm <- glm(Loan_Status ~ ., data=data.train, family='binomial')
fit.glm %>% summary

### 검정자료 예측
pred.prob <- predict(fit.glm, newdata=data.test, type='response')
pred.prob %>% summary

pred.glm <- ifelse(pred.prob> 0.5, "Y","N") %>% factor()
pred.glm

### 정오분로표
table(data.test$Loan_Status, pred.glm)

### Cinfusion matrix and statistics
confusionMatrix(reference=data.test$Loan_Status, data=pred.glm, positive="Y")

### roc curve
roc.curve(data.test$Loan_Status, pred.glm)
roc.curve(data.test$Loan_Status, pred.prob)

# > CART: Classfication and Regression Trees
### Library
library(rpart)
library(rpart.plot)

### 모형적합
fit.rpart <- rpart(Loan_Status ~ ., data=data.train)
fit.rpart %>% summary()
### 모형확인
prp(fit.rpart, type = 2, extra = 4)

rpart.plot(fit.rpart)

### 검정자료 예측
pred.rpart <- predict(fit.rpart, data.test, type='class')

df.pred <- cbind(data.test, pred.rpart)

### 정오분류표
#table(data.test$quality, pred.rpart)
table(pred.rpart, data.test$Loan_Status)


### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$Loan_Status, data=pred.rpart, positive='Y')


### ROC curve & AUC
prob.rpart <- predict(fit.rpart, data.test, type='prob')
ROSE::roc.curve(data.test$Loan_Status, prob.rpart[,'Y'])

### 변수 중요도
fit.rpart$variable.importance

model <- train(Loan_Status ~ ., data=data.train, method='rpart')
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)

# > C4.5
library(RWeka)
library(partykit)

### 모형적합
fit.c4.5 <- J48(Loan_Status ~ ., data = data.train)
### 모형확인
plot(fit.c4.5)

### 검정자료 예측
pred.c4.5 <- predict(fit.c4.5, data.test)
### 정오분료표
#table(data.test$quality, pred.c4.5)
### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$Loan_Status, data=pred.c4.5, positive='Y')

### ROC curve & AUC
ROSE::roc.curve(data.test$Loan_Status, prob.rpart[,'Y'])

prob.c4.5 <- predict(fit.c4.5, data.test, type='prob')
ROSE::roc.curve(data.test$Loan_Status, prob.c4.5[,'Y'], add.roc=T, col='red', lty=2, lwd=2)


### Random forest
### 모형 적합
fit.rf <- randomForest(Loan_Status ~., data=data.train, ntree=200, importance=T)

### 검정자료 예측
pred.rf <- predict(fit.rf, data.test)

### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$Loan_Status, data = pred.rf,
                       positive='Y', mode = 'everything')

### ROC curve & AUC
prob.rf <- predict(fit.rf, data.test, type='prob')
ROSE::roc.curve(data.test$Loan_Status, prob.rf[,'Y'])

### 중요도 계산
importance(fit.rf)

### 중요eh plot
varImpPlot(fit.rf, main='importance')

data.train %>% is.na() %>% sum()

library(e1071)

### 모형적합
fit.svm <- svm(Loan_Status ~ ., data=data.train,
               type='C-classification', kernel='radial',
               probability=TRUE,
               gamma=0.1, cost=1)

### 정오분류표
### 검정자료 예측
pred.svm <- predict(fit.svm, data.test)
### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$Loan_Status, data=pred.svm, positive='Y')


### ROC curve & AUC
pred.svm <- predict(fit.svm, data.test, probability = T)
prob.svm <- pred.svm %>% attr('probabilities')
ROSE::roc.curve(data.test$Loan_Status, prob.svm[,'Y'], main="ROC curve - SVM")

library(kernlab)
### 중요도 계산
model <- train(Loan_Status ~ ., data=data.train, method="svmRadial")
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)


### 파라미터 튜닝
# 하이퍼 파라미터 격자 설정
paramGrid <- expand.grid(gamma = c(0.01, 0.1, 1), cost = c(1, 10, 100))

# kfold 교차 검증 설정
ctrl <- trainControl(method = "cv", number = 10)

# 모델 훈련 (Grid Search)
fit.svm <- train(Loan_Status ~ ., data = data.train, 
                 method = "svmRadial", 
                 paramGrid = paramGrid, 
                 trControl = ctrl)

# 최적 하이퍼 파라미터 확인
print(fit.svm$bestTune)

# 최적 모델 평가
pred.svm <- predict(fit.svm, data.test)
confusionMatrix(reference = data.test$Loan_Status, data = pred.svm, positive = 'Y')
