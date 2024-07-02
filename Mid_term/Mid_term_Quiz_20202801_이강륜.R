### k-최근접 이웃(k-nearest neighbor) 모형에 대하여 조사하시오.
# 1. 알고리즘 설명
# KNN 알고리즘은 지도 학습의 일종이며, 분류모델이다.
# 한글로 K-최근접 이웃으로 예측값을 기준으로 K값에 따라 이웃한 데이터와의 유클리디언 거리를 계산해 근접한 데이터수가 많은 클래스로 분류 되는 방식이다.

# 2. 장점과 단점
## 장점 
# - 알고리즘 자체가 굉장히 간단하다.
# - 별도의 모델이 사전학습 할 필요가 없다.
# - 이진분류, 다중분류 두 경우 모두 적용할 수 있다.
# - 거리를 계산하기 때문에 수치형 데이터를 다룰 때 성능이 좋다.

## 단점
# - 데이터가 커지면 일일히 계산해야 하므로 속도가 느려진다.
# - 데이터간의 유클리디언 거리 즉 물리적인 거리를 측정하기 때문에 정규화 작업을 거쳐야 한다.
# - k값이 커지면 과소적합이 일어나고 작아지면 과대적합이 일어나기 때문에 적절한 k값을 찾는 것이 중요하다.
# - 모델이 없기 때문에 해석에 어려움이 있을 수 있다.
#
# 3. Examples in r
# - 훈련자료: winequality_red_train.csv
# - 검정자료: winequality_red_test.csv
# - 성능지표: accuracy, F1-score, AUC

### 훈련자료: winequality_red_train.csv
data.train <- read.csv('./data/winequality_red_train.csv', header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header = T, stringsAsFactors = T)


### 데이터 구조 확인
data.train %>% is.na() %>% sum()
data.train %>% summary
data.tarin %>% sapply(class)

### pre-processing
### normalize
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

data.train[1:11] <- lapply(data.train[1:11], normalize)
data.test[1:11] <- lapply(data.test[1:11], normalize)

train_drop <- data.train[1:11]
test_drop <- data.test[1:11]



library(class)

# grid search cv
cv <- trainControl(method='cv', number = 30, verbose = T)

knn.grid = expand.grid(.k = c(1:25))

train.knn <- train(quality ~ ., data.train, method = 'knn',
                   trControl = cv, tuneGrid = knn.grid)

train.knn$results
train.knn$bestTune # 1
predict.knn <- predict(train.knn, test_drop)
confusionMatrix(predict.knn, data.test$quality, positive = 'Good', mode = 'everything')


### ROC curve & AUC
prob.knn <- predict(train.knn, data.test, type='prob')
ROSE::roc.curve(data.test$quality, prob.knn[,'Good'])


best_score <- train.knn
### best AUC: 0.7422
### best F1: 0.7669
### best AUC: 0.797