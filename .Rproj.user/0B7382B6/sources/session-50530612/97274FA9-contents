### Library
library(dplyr)
# install.packages( c("arules","arulesViz") )
library(arules)
library(arulesViz)

data.file <- 'data/datasets_205531_450835_Market_Basket_Optimisation.csv'
data.raw <- readLines(data.file)
data.raw %>% head()

### 자료 처리
data.use <- strsplit(data.raw,",")
data.use %>% head()

### transactions
data.trans <- as(data.use, "transactions")

data.trans %>% summary()

### 연관규칙 생성 – apriori
data.rules <- apriori(
  data.trans, 
  para=list(
    minlen=3,             # 최소 품목의 수
    maxlen=20,          # 최대 품목의 수
    support=0.01,       # 최소 지지도
    confidence=0.5      # 최소 신뢰도
  )
)

data.rules %>% summary()

### 연관규칙 확인
inspect(data.rules)

### 연관규칙 시각화
plot(data.rules, method="scatterplot")
plot(data.rules, method="graph", engine="interactive")
