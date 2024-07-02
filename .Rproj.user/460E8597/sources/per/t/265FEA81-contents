### Packages
library(caret)
library(dplyr)
library(xgboost)

### 훈련자료: winequality_red_train.csv
data.train <- read.csv('./data/winequality_red_train.csv', header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header = T, stringsAsFactors = T)


X_train <- data.train %>% select(-quality)
y_train <- data.train$quality %>% as.numeric(.)-1
X_test <- data.test %>% select(-quality)
y_test <- data.test$quality %>% as.numeric(.)-1

### DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

watchlist <- list(train = dtrain, test = dtest)

fit.xgb.train <- xgb.train(data = dtrain, 
                   max.depth = 4, 
                   eta = 0.1, 
                   nthread = 4, 
                   nrounds = 100, 
                   objective = "binary:logistic",
                   early_stopping_rounds = 10,
                   watchlist = watchlist)

fit.xgb.train$best_iteration
fit.xgb.train$params

fit.xgb.best <- xgboost(data = dtrain,
                        params = fit.xgb.train$params,
                        nrounds = fit.xgb.train$best_iteration)

### View the trees from a model
# install.packages("DiagrammeR")
library(DiagrammeR)
xgb.plot.tree(model = fit.xgb.best)


### 검정자료 예측
prob.xgb <- predict(fit.xgb.best, dtest)

pred.xgb <- ifelse(prob.xgb > 0.5, "Good", "Bad") %>% factor()

### Confusion Matrix and Statistics
caret::confusionMatrix(reference=data.test$quality, data=pred.xgb, 
                       positive="Good", mode = "everything")

### ROC curve & AUC
ROSE::roc.curve(data.test$quality, prob.xgb)

