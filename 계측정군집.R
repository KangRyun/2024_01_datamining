### Library
library(caret)
library(dplyr)

### 파일 선택
data.file <- 'data/대전교통공사_시간대별 승하차인원_20240331.csv'

### CSV 파일 읽기
data.raw <- read.csv(data.file, header=T, stringsAsFactors=T, fileEncoding = "CP949")
data.raw %>% dim()

### 자료 추출: 구분=="승차", 6~24시
data.in <- data.raw %>%
  subset(구분=="승차") %>%
  dplyr::select(역명, X06.07시:X23.00시)

### 역별 승차인원 평균
data.mean <- sapply(data.in[-1],function(x) tapply(x, data.in$역명, mean))
data.mean %>% head()

### 분산이 큰 시간대 변수 5개 추출
data.var <- apply(data.mean, 2, var) %>% sort(T)
data.names <- names(data.var[1:5])
data.use <- data.mean[,data.names]
data.use %>% head()

### 표준화 변환
data.pre.pro <- preProcess(data.use, method = c("center", "scale"))
data.std <- predict(data.pre.pro, data.use)
data.std %>% summary()

### 거리계산 - euclidean
d <- dist(data.use, method="euclidean")
### 모형 적합 - average
fit.hclust <- hclust(d, method="average")
### Dendrogram
plot(fit.hclust)

plot(fit.hclust, hang=-1)

### 그룹 수 지정
n.group <- 3
### Dendrogram
plot(fit.hclust, hang=-1)
rect.hclust(fit.hclust, k=n.group, border="red")

### 군집 저장
groups <- cutree(fit.hclust, k=n.group)
table(groups)

### 시각화 
#install.packages("ape")
library(ape)

### 색상
colors <- rainbow(n.group)
### 1
plot(as.phylo(fit.hclust), tip.color=colors[groups], type="phylogram")

### 2
plot(as.phylo(fit.hclust), tip.color=colors[groups], type="cladogram")

### 3
plot(as.phylo(fit.hclust), tip.color=colors[groups], type="fan")

### 4
plot(as.phylo(fit.hclust), tip.color=colors[groups], type="unrooted")

### 그룹별 평균
data.result.mean <- apply(data.use, 2, function(x) tapply(x, groups, mean))
data.result.mean

### 군집 그림
plot(data.use[,1], data.use[,2], type="n", xlab="", ylab="")
points(data.result.mean[,1], data.result.mean[,2], col=1:3, pch=8, cex=1.5)
text(data.use[,1], data.use[,2], rownames(data.use), col=groups)

### 자료 구조 변경
data.out <- cbind(data.use, groups) %>% data.frame()
data.melt <- reshape2::melt(data.out, "groups")
data.melt %>% head()

### Plotly
library(plotly)

plot_ly(data.melt, x = ~groups, y = ~value, split = ~variable, type = "box") %>% layout(boxmode = "group")