### jre 설치경로 선택
java.path <- choose.dir()
java.path
# 
# ### 황경 변수 등록
Sys.setenv(JAVA_HOME=java.path)
# 
install.packages("rJava")
# 
library(rJava)
library(dplyr)
library(caret)

### Load Data
data(iris)
data.raw <- iris

### 자료 확인
dim(data.raw)
head(data.raw)
summary(data.raw)
data.raw %>% sapply(class)


### 훈련자료의 비율
train.ratio <- 0.6

### sampling 할때는 추출한 샘플을 파일로 저장 
### 훈련자료
set.seed(42)
train.index <- sample(1:nrow(data.raw), nrow(data.raw)*train.ratio)
data.train <- data.raw[train.index,]
dim(data.train)

### 검정자료
data.test <- data.raw[-train.index,]
dim(data.test)

