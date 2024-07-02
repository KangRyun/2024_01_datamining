# `data_2009.csv` 자료를 이용하여 `Destination_city=Seattle, WA`인 `Origin_city` 2곳을 제시하시오. - 연관규칙(Association Rule) 사용
# - 최소 지지도(support) = 0.001
# - 최소 신뢰도(confidence) = 0.5

library(dplyr)
library(arules)
library(arulesViz)


data.raw <- read.csv('data/data_2009.csv')

### transactions
data.trans <- as(data.raw, "transactions")
data.trans %>% summary()


### 연관규칙 생성 – apriori
data.rules <- apriori(
  data.trans, 
  para=list(
    support=0.001,       # 최소 지지도
    confidence=0.5      # 최소 신뢰도
  )
)
data.rules %>% summary()

### 연관규칙 확인
inspect(data.rules)


### {Origin_city=Fairbanks, AK} => {Destination_city=Seattle, WA} support = 0.001223812 confidence = 0.9767442
### {Origin_city=Anchorage, AK} => {Destination_city=Seattle, WA} support = 0.003525744 confidence = 0.8013245