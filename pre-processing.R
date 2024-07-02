library(dplyr)
library(caret)

data_file <- file.choose()
data_file
# "C:\\Users\\kr937\\Desktop\\drive\\2024\\SS\\data_mining\\24_Data_mining\\우리동네 대기 정보(한남대학교).csv"


data_raw <- read.csv(data_file, header = T)
data_raw %>% dim()
data_raw %>% head()

data_raw %>% summary()

### 한개의 값만 가지는 변수 제거
### 분산이 0인 변수를 찾아서 제거함
data_nzv <- caret::nearZeroVar(data_raw, saveMetrics = TRUE)
### data_nzv의 타입 확인
data_nzv %>% mode()
data_nzv

!data_nzv$zeroVar

data_use <- data_raw[,!data_nzv$zeroVar]
data_use %>% head()

# 중복변수 제거
## 상관관계를 이용한 중복된 변수를 제거(상관계수=1)
## 일부 모형에서는 입력변수들 사이에 높은 상관관계가 존재하면 모형이 불안정해지는 경향이 있음
### |상관계수| > 0.7

# 통계모형은 각각 변수를 독립이라 생각하기 때문에 두 변수가 유사하면 불안한 결과가 나올 수가 있음
# 기계학습모형에서는 두 변수가 유사해도 상관이 없다.
# 때문에 변수의 특징을 파악하야 한다.

### 날짜 변수 제거 후 저장
data_use_1 <- data_use[-1]

### 상관행렬
data_use_cor <- data_use_1 %>% cor()

### 상관계수 0.7 이상인 열 인덱스 계산
highlyCorIndex <- findCorrelation(data_use_cor, cutoff = 0.7)
highlyCorIndex

### 상관계수가 0.7 이상인 열 제거
data_use_2 <- data_use_1[-highlyCorIndex]
data_use_2 %>% cor()

## 변수제거에 있어서는 목적에 맞게 해야 정석이다.
## 즉 1퍼센트도 올리기 위해 모든걸 해봐야 한다.
### 범주형 데이터 간의 척도를 볼려면 유사도를 봐야한다
## 결국 데이터의 특성, 타입, 유형을 파악해야한다

# 변수의 스케일 변환
## 입력 또는 출력변수의 값을 표준화(평균=0, 표준편차=1)하거나, 0과 1사이의 값을 가지도록 스케일
## 변환이 필요
### {cater} 패키지의 preProcedd() 함수에서 method를 "center", "scale", "range", "BoxCox" 등으로
### 설정하고, predict() 함수로 값을 변환

### 표준화를 하지 않고 예측 결과가 좋다면 써도 되지만 변수 변환을 한 모형과 비교를 해야한다.

### 표준화 변환
data_pre_pro <- caret::preProcess(data_use_2, method = c("center", "scale"))
data_use_std <- predict(data_pre_pro, data_use_2)

### 변수별 평균과 표준편차 확인
sapply(data_use_std, mean, na.rm=T) %>% round(2)
sapply(data_use_std, sd, na.rm=T) %>% round(2)

### 범위 변환
data_pre_pro <- caret::preProcess(data_use_2, method = c("range"))
data_use_rng <- predict(data_pre_pro, data_use_2)

data_use_rng %>% summary()

# 결측값(missing value) 처리
## 결측값을 처리하는 방법
### 무시
#### 별도의 처리 없이 사용
#### 분석 단계에서 처리
### 제거
#### 측값이 많이 포함된 변수(열)를 제거
#### 측값이 한 개 이상 존재하는 경우 자료(행)를 제거
### 대체(imputation)
#### 존재하는 값을 기준으로 새로운 값을 생성
#### 변수를 기준으로 평균 또는 중위수 등을 계산하여 대체
#### 자료를 기준으로 유사한 자료를 선택하여 대체


### 결측값이 한 개 이상 존재하는 경우 자료(행)를 제거
data_use_2 %>% dim()
# 8개의 행 제거
na.omit(data_use_2) %>% dim()
na.omit(data_use_2) %>% summary()

### 결측값 대치(imputation)
#### k-nearest neighbor imputation
### Library
# install.packages("RANN")
library(RANN)

### 결측값 대치 + 범위 변환
data_pre_pro <- caret::preProcess(data_use_2, method=c("knnImpute", "range"))
data_use_imp <- predict(data_pre_pro, data_use_2)

### 요약자료 확인
data_use_std %>% summary()
data_use_std
data_use_imp %>% summary()
data_use_imp


### Library
# install.packeages("fastDummies")
library("fastDummies")
data("PlantGrowth")

PlantGrowth
unique(PlantGrowth$group)

### group 변수로 더미변수 생성
results <- dummy_cols(PlantGrowth, select_columns = "group")
results

### group 변수로 더미변수 생성 - 첫 번째 더미변수 제거
results <- dummy_cols(PlantGrowth, 
                      select_columns = "group", 
                      remove_first_dummy = T)
results
