library(dplyr)
library(ROSE)
library(caret)


### 훈련자료: winequality_red_wine_train.csv
data.train <- read.csv('./data/heart_train.csv', header=T, stringsAsFactors=T)
### 검정자료: winequality_red_wine_test.csv
data.test <- read.csv('./data/heart_test.csv', header=T, stringsAsFactors=T)
# age: 회원의 나이 
# sex: 사람의 성별 
# cp: 흉통 유형 값 1: 전형적인 협심증 값 2: 비정형 협심증 값 3: 비협심증 값 4: 무증상 
# trtbps: 안정 시 혈압(mm Hg) 
# chol: BMI 센서를 통해 가져온 콜레스테롤(mg/dl)
# fbs: (공복 혈당 > 120 mg/dl) (1 = 참, 0 = 거짓) 
# restecg: 휴식 심전도 결과 값 0: 정상 값 1: ST-T 파 이상(T 파 역전 및/또는 0 이상의 ST 상승 또는 하강)이 있음.05mV) 값 2: 에스테스의 기준에 따라 좌심실 비대 가능성이 있거나 확실함 
# thalachh: 최대 심박수 달성 
# exng: 운동 유발 협심증(1 = 예, 0 = 아니오) 
# oldpeak: 이전 최고점 
# slp: 경사도 
# caa: 주요 혈관 수(0-3) 
# thall: 탈 속도 
# output: 목표 변수(0=심장마비 가능성 낮음, 1=심장마비 가능성 높음)
data.train %>% head()
data.train %>% dim()
data.train %>% sapply(class)

### 정수형 데이터를 factor로 변환
data.train$output <- as.factor(data.train$output)
data.test$output <- as.factor(data.test$output)

### seed 설정
set.seed(42)


# 로지스틱 회귀모형 ---------------------------------------------------------------
# 로지스틱
fit.glm <- glm(output ~ ., data=data.train, family='binomial')
fit.glm %>% summary

### 검정자료 예측
pred.prob <- predict(fit.glm, newdata=data.test, type='response')
pred.prob %>% summary

pred.glm <- ifelse(pred.prob> 0.5, "1","0") %>% factor()
pred.glm

### 정오분로표
table(data.test$output, pred.glm)

### Confusion matrix and statistics
confusionMatrix(reference=data.test$output, data=pred.glm, positive="1", mode = 'everything')

### roc curve
roc.curve(data.test$output, pred.prob)



# CART: Classfication and Regression Trees --------------------------------
### Library
library(rpart)
library(rpart.plot)

### 모형적합
fit.rpart <- rpart(output ~ ., data=data.train)
fit.rpart %>% summary()
### 모형확인
prp(fit.rpart, type = 2, extra = 4)

rpart.plot(fit.rpart)

### 검정자료 예측
pred.rpart <- predict(fit.rpart, data.test, type='class')

df.pred <- cbind(data.test, pred.rpart)

### 정오분류표
#table(data.test$quality, pred.rpart)
table(pred.rpart, data.test$output)


### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$output, data=pred.rpart, positive='1', mode = 'everything')


### ROC curve & AUC
prob.rpart <- predict(fit.rpart, data.test, type='prob')
ROSE::roc.curve(data.test$output, prob.rpart[,'1'])

### 변수 중요도
fit.rpart$variable.importance

model <- train(output ~ ., data=data.train, method='rpart')
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)


# C4.5 --------------------------------------------------------------------
library(RWeka)
library(partykit)

### 모형적합
fit.c4.5 <- J48(output ~ ., data = data.train)
### 모형확인
plot(fit.c4.5)

### 검정자료 예측
pred.c4.5 <- predict(fit.c4.5, data.test)
### 정오분료표
#table(data.test$quality, pred.c4.5)
### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$output, data=pred.c4.5, positive='1', mode = 'everything')

### ROC curve & AUC
ROSE::roc.curve(data.test$output, prob.c4.5[,'1'])


# SVM ---------------------------------------------------------------------
library(e1071)

### 모형적합
fit.svm.lin <- svm(output ~ ., data=data.train,
               type='C-classification', kernel='linear',
               probability=TRUE,
               gamma=0.1, cost=1)

fit.svm.pol <- svm(output ~ ., data=data.train,
               type='C-classification', kernel='polynomial',
               probability=TRUE,
               gamma=0.1, cost=1)

fit.svm.rad <- svm(output ~ ., data=data.train,
               type='C-classification', kernel='radial',
               probability=TRUE,
               gamma=0.1, cost=1)


### 정오분류표
### 검정자료 예측
pred.svm.lin <- predict(fit.svm.lin, data.test)
pred.svm.pol <- predict(fit.svm.pol, data.test)
pred.svm.rad <- predict(fit.svm.rad, data.test)


### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$output, data=pred.svm.lin, positive='1', mode = 'everything')
caret::confusionMatrix(reference=data.test$output, data=pred.svm.pol, positive='1', mode = 'everything')
caret::confusionMatrix(reference=data.test$output, data=pred.svm.rad, positive='1', mode = 'everything')


### ROC curve & AUC
pred.svm.lin <- predict(fit.svm.lin, data.test, probability = T)
pred.svm.pol <- predict(fit.svm.pol, data.test, probability = T)
pred.svm.rad <- predict(fit.svm.rad, data.test, probability = T)

prob.svm.lin <- pred.svm.lin %>% attr('probabilities')
prob.svm.pol <- pred.svm.pol %>% attr('probabilities')
prob.svm.rad <- pred.svm.rad %>% attr('probabilities')

ROSE::roc.curve(data.test$output, prob.svm.lin[,'1'], main="ROC curve - SVM")
ROSE::roc.curve(data.test$output, prob.svm.pol[,'1'], main="ROC curve - SVM")
ROSE::roc.curve(data.test$output, prob.svm.rad[,'1'], main="ROC curve - SVM")

library(kernlab)
### 중요도 계산
model <- train(output ~ ., data=data.train, method="svmLinear")
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)


# 나이브 베이즈 -----------------------------------------------------------------
### 모형 적합
fit.naive <- naiveBayes(output ~ ., data=data.train)


### 검정자료 예측
pred.naive <- predict(fit.naive, data.test)

### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$output, data=pred.naive, positive="1", mode = 'everything')

### ROC curve & AUC
prob.naive <- predict(fit.naive, data.test, type="raw")
ROSE::roc.curve(data.test$output, prob.naive[,"1"])

### 중요도 계산
#install.packages("klaR")
library(klaR)

model <- train(output ~ ., data=data.train, method="nb")
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)


# RNN ---------------------------------------------------------------------
library(nnet)
### 변수의 스케일 변환 - 표준화 변환
data.pre.pro <-  caret::preProcess(data.train, method = c("center", "scale"))
data.train <- predict(data.pre.pro, data.train)
data.test <- predict(data.pre.pro, data.test)

### 모형적합
fit.nnet <- nnet(output ~ ., data=data.train, size=10, maxit=1000)

### plot
NeuralNetTools::plotnet(fit.nnet)

### 검정자료 예측### 검정자료 예측### 검정자료 예측
pred.nnet <- predict(fit.nnet, data.test, type='class') %>% factor()

### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$output, data=pred.nnet, 
                       positive='1', mode = 'everything')

### ROC curve & AUC
prob.nnet <- predict(fit.nnet, data.test, type='raw')
ROSE::roc.curve(data.test$output, prob.nnet[,1])

### 중요도 계산
library(NeuralNetTools)
NeuralNetTools::garson(fit.nnet) + coord_flip()

NeuralNetTools::olden(fit.nnet) + coord_flip()


# random_forest -----------------------------------------------------------
library(randomForest)

### 훈련자료: winequality_red_wine_train.csv
data.train <- read.csv('./data/heart_train.csv', header=T, stringsAsFactors=T)
### 검정자료: winequality_red_wine_test.csv
data.test <- read.csv('./data/heart_test.csv', header=T, stringsAsFactors=T)

### 정수형 데이터를 factor로 변환
data.train$output <- as.factor(data.train$output)
data.test$output <- as.factor(data.test$output)

### 모형 적합
fit.rf <- randomForest(output ~., data=data.train, ntree=250, importance=T)

### 검정자료 예측
pred.rf <- predict(fit.rf, data.test)

### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$output, data = pred.rf,
                       positive='1', mode = 'everything')

### ROC curve & AUC
prob.rf <- predict(fit.rf, data.test, type='prob')
ROSE::roc.curve(data.test$output, prob.rf[,'1'])

### 중요도 계산
importance(fit.rf)

### 중요eh plot
varImpPlot(fit.rf, main='importance')


# 2. 위의 모형 평가 지표 중 AUC 기준으로 최종 모형을 선택하고, 모형의 결과를 해석하시오. [10점] -------------
# 최종 모형은 나이브 베이즈 분류 모델입니다.
# 변수중요도를 보았을 때 전반적으로 변수들이 심장마비의 영향을 미치지만 그중에 fbs 즉 공복혈당은 큰 인과관계를 보이지는 않습니다.
# cp, caa, oldpeak 세개의 변수들이 심장마비의 가능성에 크게 미치는 것을 확인할 수 있습니다
# 변수중요도의 임계값을 0.7로 설정한다고 가정하면 추가적으로 thalachh, thall 변수도 심장마비 가능성에 영향을 미치는 변수라고 설명할 수 있습니다.