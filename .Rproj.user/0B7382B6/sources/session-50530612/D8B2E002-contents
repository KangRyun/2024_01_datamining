### library
library(dplyr)
library(igraph)
library(ggraph)

getwd()
df <- read.csv('Final_quiz/kci_hannam.csv', fileEncoding = 'cp949')
df_2 <- df %>% filter(Freq >= 2)

ggraph(df_2, layout = "fr") + 
  geom_edge_link(aes(width = Freq, edge_alpha = Freq), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  scale_edge_alpha_continuous(range = c(0.6, 1)) +
  theme(legend.position = "none")

## 윤용식 교수에 대한 결과
df_2 %>% filter(저자 == '윤용식' | 공동저자 == '윤용식')

## 권선영 교수에 대한 결과
df_2 %>% filter(공동저자 == '권선영' | 저자=='권선영')

# 최인식 교슈에 대한 결과
df_2 %>% filter(저자 == '최인식' | 공동저자=='최인식')



### 분석 결과
# 윤용식 교수는 권성준 교수와 총 8편의 논문을 함께 투고 하였음으로 강한 사회망을 구축하고 있다
# 권선영 교수는 김영주, 김지수 교수와 총 9편의 논문을 투고하였고 제1저자가 아닌 공동저자로써 논문에 참여하였다. 따라서 주도적으로 논문을 작성하기 보다는 다른 교수의 논문을 도와준다고 볼수 있다.
# 또 다른 사례로는 최인식 교수도 김용빈, 최영재 교수와의 사회적 관계도 비슷한 사례로 볼 수 있다.

# 더 많은 강한 관게를 가지는 교수들이 있으나 이 노드간 군집의 특성을 파악할 필요가 있다. 추가적으로 어떠한 군집이 어느 학과별로 교수들간의 군집을 형성하고 있는지 확인해야 할 필요가 있다.
