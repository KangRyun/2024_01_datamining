library(dplyr)
library(caret)


### data load
data.raw <- read.csv('./data/대전교통공사_시간대별 승하차인원_20240331.csv', header=T, stringsAsFactors=T, fileEncoding = 'cp949')

data.raw %>% dim()
data.raw %>% head()
data.raw %>% str()
data.raw %>% summary()

### 자료 추출: 구분 == '승차', 6-24시
data.in <- data.raw %>% 
  subset(구분='승차') %>% 
  select(역명, X06.07시:X23.00시)

### 역별 승차 인원 평균
data.mean <- sapply(data.in[-1],function(x) tapply(x, data.in$역명, mean))
data.mean %>% head()

### 분산이 큰 변수 5개 추출
data.var <- apply(data.mean, 2, var) %>% sort(T)
data.names <- names(data.var[1:5])
data.use <- data.mean[,data.names]
data.use %>% head()

### 표준화 변환
data.pre.pro <- preProcess(data.use, method = c('center', 'scale'))
data.std <- predict(data.pre.pro, data.use)
