library(tidygraph)
library(ggraph)
library(corrplot)
library(rstatix)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readxl)
library(survival) 
library(ggfortify)
library(survminer)
library(rlang)
library(janitor)
library(tidyverse)
library(igraph)

normal_test_no_p <- normal_test %>%
  select(c(-1,-6))

normal_graph <- as_tbl_graph(normal_test_no_p, directed = FALSE)

disease_test_no_p <- disease_test %>%
  select(c(-1,-6))

disease_graph <- as_tbl_graph(disease_test_no_p, directed = FALSE)

normal_hub <- normal_graph %>%
  activate(nodes) %>%
  mutate(importance = centrality_hub(weights = normal_test_no_p$cor)) %>%
  as.data.frame()

important_normal_hub <- filter(normal_hub, importance > 0.80)

disease_hub <- disease_graph %>%
  activate(nodes) %>%
  mutate(importance = centrality_hub(weights = disease_test_no_p$cor)) %>%
  as.data.frame()

important_disease_hub <- filter(disease_hub, importance > 0.85)

write.csv(important_normal_hub, file = "important_normal_hub.csv")
write.csv(important_disease_hub, file = "important_disease_hub.csv")


normal_test_no_p <- mutate(normal_test_no_p, cor = abs(normal_test_no_p$cor))

normal_betweenness <- normal_graph %>%
  activate(nodes) %>%
  mutate(importance = centrality_betweenness(weights = normal_test_no_p$cor)) %>%
  as.data.frame()

important_normal_betweenness <- filter(normal_betweenness, importance > 4)


disease_test_no_p <- mutate(disease_test_no_p, cor = abs(disease_test_no_p$cor))

disease_betweenness <- disease_graph %>%
  activate(nodes) %>%
  mutate(importance = centrality_betweenness(weights = disease_test_no_p$cor)) %>%
  as.data.frame()

important_disease_betweenness <- filter(disease_betweenness, importance > 4)

write.csv(important_normal_betweenness, file = "important_normal_betweenness.csv")
write.csv(important_disease_betweenness, file = "important_disease_betweenness.csv")

normal_group <- normal_graph %>%
  activate(nodes) %>%
  mutate(group = group_edge_betweenness(weights = normal_test_no_p$cor)) %>%
  as.data.frame()

important_normal_group_1 <- filter(normal_group, normal_group$group == 1)
write.csv(important_normal_group_1, file = "important_normal_group_1.csv")
important_normal_group_1 <- c(important_normal_group_1$name)

important_normal_group_2 <- filter(normal_group, normal_group$group == 2)
write.csv(important_normal_group_2, file = "important_normal_group_2.csv")
important_normal_group_2 <- c(important_normal_group_2$name)

important_normal_group_3 <- filter(normal_group, normal_group$group == 3)
write.csv(important_normal_group_3, file = "important_normal_group_3.csv")
important_normal_group_3 <- c(important_normal_group_3$name)

important_normal_group_4 <- filter(normal_group, normal_group$group == 4)
write.csv(important_normal_group_4, file = "important_normal_group_4.csv")
important_normal_group_4 <- c(important_normal_group_4$name)


disease_group <- disease_graph %>%
  activate(nodes) %>%
  mutate(group = group_edge_betweenness(weights = disease_test_no_p$cor)) %>%
  as.data.frame()

important_disease_group_1 <- filter(disease_group, disease_group$group == 1)
write.csv(important_disease_group_1, file = "important_disease_group_1.csv")
important_disease_group_1 <- c(important_disease_group_1$name)

important_disease_group_2 <- filter(disease_group, disease_group$group == 2)
write.csv(important_disease_group_2, file = "important_disease_group_2.csv")
important_disease_group_2 <- c(important_disease_group_2$name)


normal_network <- ggraph(normal_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = normal_hub$importance) +
  geom_node_text(aes(label = name, color = normal_group$group), size = normal_betweenness$importance, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave("normal_network.png", normal_network, width = 30, height = 30, units="cm")

disease_network <- ggraph(disease_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = disease_hub$importance) +
  geom_node_text(aes(label = name, color = disease_group$group), size = disease_betweenness$importance, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave("disease_network.png", normal_network, width = 30, height = 30, units="cm")
