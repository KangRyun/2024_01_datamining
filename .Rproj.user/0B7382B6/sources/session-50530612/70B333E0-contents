### Library
library(caret)
library(dplyr)
library(fpc)
library(dbscan)

### 파일 선택
data.file <- 'data/대전교통공사_시간대별 승하차인원_20240331.csv'

### CSV 파일 읽기
data.raw <- read.csv(data.file, header=T, stringsAsFactors=T, fileEncoding = "CP949")
data.raw %>% dim()

### 분산이 큰 시간대 변수 5개 추출
data.var <- apply(data.mean, 2, var) %>% sort(T)
data.names <- names(data.var[1:5])
data.use <- data.mean[,data.names]
data.use %>% head()


### 표준화 변환
data.pre.pro <- preProcess(data.use, method = c("center", "scale"))
data.std <- predict(data.pre.pro, data.use)
data.std %>% summary()

### DBSCAN 모형 적합
fit.dbscan <- dbscan(data.std, eps=1.5, MinPts=3)
fit.dbscan

### 군집 저장
groups <- fit.dbscan$cluster
table(groups)

### 그룹별 평균
data.result.mean <- apply(data.std, 2, function(x) tapply(x, groups, mean))
data.result.mean

### 군집 그림
plot(data.std[,1], data.std[,2], type="n", xlab="", ylab="")
text(data.std[,1], data.std[,2], rownames(data.std), col=fit.km$cluster)

points(data.result.mean[,1], data.result.mean[,2], col=1:3, pch=8, cex=1.5)

### 자료 구조 변경
data.out <- cbind(data.std, groups) %>% data.frame()
data.melt <- reshape2::melt(data.out, "groups")
data.melt %>% head()

### Plotly
library(plotly)
plot_ly(data.melt, x = ~groups, y = ~value, split = ~variable, type = "box") %>% layout(boxmode = "group")
