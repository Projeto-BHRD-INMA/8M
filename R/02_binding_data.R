# Script to read additional information of female researchers
# binds both data and makes graphics
# by Sara Mortara, first version 2020-02-27

## loading packages
library(dplyr)

## loading files
fa <- read.csv("outputs/female_area_2.csv")
fs <- read.csv("outputs/female_science_2.csv")

# 1. juntando os dados ####
# juntando as duas tabelas, e removendo as duplicadas
fall <- bind_rows(fa, fs, .id = 'id') %>%
  .[!duplicated(.[,-1]), ]

# alterando o id, 1 = cientistas da área e 2 = cientistas geral
fall$id <- ifelse(fall$id == 1, "area", "geral")
head(fall)

# escrevendo o output
# write.csv(fall, "outputs/02_all_women.csv",
#           row.names = FALSE)


