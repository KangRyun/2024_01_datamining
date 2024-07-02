library(dplyr)
library(igraph)
library(ggraph)


data.raw <- read.csv("./data/서울특별시_지하철 역별 OD.csv", header = T, fileEncoding = "CP949")
data.raw %>% dim()

data.raw %>% summary()

### 자료처리
data.use <- data.raw %>% 
  filter(승차_호선=="2호선" & 하차_호선=="2호선") %>% 
  filter(총_승객수>2000) %>% 
  select(승차_역, 하차_역, 총_승객수)
data.use %>% dim()


### 그래프 자료로 저장
data.g <- graph_from_data_frame(data.use)
### 연결망 그래프: plot.igraph()
plot(data.g, edge.arrow.size=0.5, edge.curved=0.2)


### 연결망 그래프 - 링크 가충치
E(data.g)$width <- data.use$총_승객수/1000
plot(data.g, edge.curved=0.2)


ggraph(data.g, layout = 'graphopt') + 
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) + 
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), col = "gray50", size = 4) +
  theme(legend.position = "none")


ggraph(data.g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) + 
  geom_node_point(size = 3) + 
  coord_fixed() +
  geom_node_text(aes(label = name), col = "gray50", size = 4) +
  theme(legend.position = "none")

ggraph(data.g, layout = 'graphopt') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name),
                     width = 총_승객수, edge_alpha = 총_승객수), 
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_text(aes(label = name))


ggraph(data.g, layout = "fr") + 
  geom_edge_link(aes(width = 총_승객수, edge_alpha = 총_승객수), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme(legend.position = "none")


ggraph(data.g, layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(width = 총_승객수, edge_alpha = 총_승객수), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme(legend.position = "none") +
  coord_fixed()
