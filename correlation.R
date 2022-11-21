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

cor_normal <- cor_mat(results_normal, method = "spearman")
cor_disease <- cor_mat(results_disease, method = "spearman")

plot_n <- cor_normal %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot()

plot_d <- cor_disease %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot()

test_normal <- cor_test(results_normal,  method = "spearman") 

normal_test <- test_normal %>%
  filter(p<0.05)

test_disease <- cor_test(results_disease,  method = "spearman")

disease_test <- test_disease %>%
  filter(p<0.05)

write.csv(normal_test, file = "normal_test.csv")
write.csv(disease_test, file = "disease_test.csv")