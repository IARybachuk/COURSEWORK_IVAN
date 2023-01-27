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
library(cluster)    
library(factoextra) 
library(dendextend)

results_normal_2 <- results_normal%>%
  select(-1) %>%
  t() %>%
  as.data.frame()

normal_matrix <- dist(results_normal_2, method = "euclidean")
normal_clust<- hclust(normal_matrix, method = "complete")

results_disease_2 <- results_disease%>%
  select(-1) %>%
  t() %>%
  as.data.frame()

disease_matrix <- dist(results_disease_2, method = "euclidean")
disease_clust<- hclust(disease_matrix, method = "complete" )

normal_dend<- as.dendrogram (normal_clust)
disease_dend <- as.dendrogram (disease_clust)

png("dend.png", width = 25, height = 20, res = 300, units = "cm")
tanglegram(normal_dend, disease_dend)
dev.off()

cor_cophenetic(normal_dend, disease_dend) #0.7457064
