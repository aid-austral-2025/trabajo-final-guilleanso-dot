# Lectura y limpieza mínima 

library(readr)   
library(dplyr)   
library(stringr) 

# 1) Ruta relativa 
ruta <- "Dataset/Dataset_Agencia.csv"

# 2) Lectura
agencia_raw <- readr::read_delim(
  file = ruta,
  delim = ";",
  na = c("", "NA"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

# 3) Limpieza mínima: espacios extra en textos
agencia <- agencia_raw %>%
  mutate(across(where(is.character), str_squish))


# Comprobaciones generales
nrow(agencia); ncol(agencia)
names(agencia)[1:10]
head(agencia, 3)