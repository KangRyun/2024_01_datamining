### Library
library(caret)
library(dplyr)

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

### Library
#install.packages('NbClust')
library(NbClust)

### determining the best number of clusters
nc <- NbClust(data.use, distance = "euclidean", max.nc=10, method="kmeans")

nc.tb <- table(nc$Best.n[1,]) %>% sort(T)
layout(matrix(1))

barplot(nc.tb)

### 그룹 수 지정
n.group <- 3
### 모형 적합
fit.km <- kmeans(data.use, n.group)
### 군집의 중심값 확인
fit.km$centers

### 군집 저장
groups <- fit.km$cluster
table(groups)

### 그룹별 평균
data.result.mean <- apply(data.use, 2, function(x) tapply(x, groups, mean))
data.result.mean

### 군집 그림
plot(data.use[,1], data.use[,2], type="n", xlab="", ylab="")
text(data.use[,1], data.use[,2], rownames(data.use), col=fit.km$cluster)

points(fit.km$centers[,1], fit.km$centers[,2], col=1:3, pch=8, cex=1.5)

### 자료 구조 변경
data.out <- cbind(data.use, groups) %>% data.frame()
data.melt <- reshape2::melt(data.out, "groups")
data.melt %>% head()

### Plotly
library(plotly)
plot_ly(data.melt, x = ~groups, y = ~value, split = ~variable, type = "box") %>% layout(boxmode = "group")