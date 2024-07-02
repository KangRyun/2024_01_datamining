### Packages
library(caret)
library(dplyr)
library(h2o)

### 훈련자료: winequality_red_train.csv
data.train <- read.csv('./data/winequality_red_train.csv', header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
data.test <- read.csv('./data/winequality_red_test.csv', header = T, stringsAsFactors = T)

# start
# invisible(h2o.init())
h2o.init()

train_h <- as.h2o(data.train)
test_h <- as.h2o(data.test)

y <- "quality"
pred <- setdiff(names(data.train), y)

aml <- h2o.automl(x = pred, y = y,
                  training_frame = train_h,
                  max_models = 10,
                  max_runtime_secs = 60)

lb <- aml@leaderboard
lb

# prediction result on test data
prediction <- h2o.predict(aml@leader, test_h[,-12]) %>% as.data.frame()
prediction

### Confusion Matrix and Statistics
caret::confusionMatrix(reference = data.test$quality, data = prediction$predict,
                       positive = 'Good', mode = 'everything')


### ROC curve & AUC
ROSE::roc.curve(data.test$quality, prediction$Good)
