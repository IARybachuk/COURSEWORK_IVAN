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

normal <- subset(clinical_data, Characteristics.organism.part.=="ovary" & Characteristics.disease.=="normal")
disease <- filter(clinical_data, 
                  str_detect(Characteristics.disease., "ovary carcinoma|ovarian adenocarcinoma|ovarian serous carcinoma|ovarian carcinoma|ovarian cancer|serous epithelial ovarian cancer|papillary serous ovarian adenocarcinoma|serous borderline ovarian cancer|endometrioid ovarian cancer|ovarian papillary serous adenocarcinoma"))

samples_normal <- normal %>%
  select(1) %>%
  unlist()
samples_normal <- as.numeric(str_extract(samples_normal, "(\\d)+"))

samples_disease <- disease %>%
  select(1) %>%
  unlist()
samples_disease <- as.numeric(str_extract(samples_disease, "(\\d)+"))

data_expr_updated <- expression %>%
  select(-1) %>%
  pivot_longer(starts_with("Sample"), names_to = "Samples", values_to = "Expression")

new_results <- data_expr_updated %>%
  group_by(`HGNC symbol`, Samples) %>%
  summarise(Expression = median(Expression, na.rm = T)) %>%
  pivot_wider(names_from = `HGNC symbol`, values_from = Expression)

results_normal <- new_results %>%
  mutate(Samples = as.numeric(str_extract(Samples, "(\\d)+"))) %>%
  filter(Samples %in% samples_normal)

results_disease  <- new_results %>%
  mutate(Samples = as.numeric(str_extract(Samples, "(\\d)+"))) %>%
  filter(Samples %in% samples_disease)

results_normal <- subset(results_normal, select = -Samples)
results_disease <- subset(results_disease, select = -Samples)


write.csv(results_normal, file = "results_normal.csv")
write.csv(results_disease, file = "results_disease.csv")
