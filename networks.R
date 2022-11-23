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

normal_test_no_p <- normal_test %>%
  select(-5)

normal_graph <- as_tbl_graph(normal_test_no_p, directed = FALSE)

network_normal <- ggraph(normal_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave(filename = "network_normal.png", 
       plot = network_normal, scale = 3)

disease_test_no_p <- disease_test %>%
  select(-5)

disease_graph <- as_tbl_graph(disease_test_no_p, directed = FALSE)

network_disease <- ggraph(disease_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave(filename = "network_disease.png", 
       plot = network_disease, scale = 3)