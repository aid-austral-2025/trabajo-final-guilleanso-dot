# LECTURA Y LIMPIEZA DEL DATASET

# Librerías que uso acá
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# 1) Ruta del archivo (relativa a la carpeta del proyecto)
ruta <- "Dataset/Dataset_Agencia.csv"

# 2) Leo el CSV (separador ";")
agencia_raw <- readr::read_delim(
  file = ruta,
  delim = ";",
  na = c("", "NA"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

# 3) Limpieza básica de textos (saco espacios dobles)
agencia <- agencia_raw |>
  mutate(across(where(is.character), str_squish))

# 4) Armo una fecha de referencia (fecha_ref)
#    - uso la primera que esté disponible (salida, alta o regreso)
#    - convierto primero con ymd; si falla, pruebo con dmy
agencia_base <- agencia |>
  mutate(
    fecha_ref_raw = coalesce(fecha_salida, fecha_alta, fecha_regreso),
    fecha_ref_raw = na_if(fecha_ref_raw, "0"),
    # me quedo sólo con la parte de fecha si viene "fecha hora"
    fecha_ref_str = str_trim(sub("\\s.*$", "", coalesce(fecha_ref_raw, ""))),
    fecha_ref = suppressWarnings(ymd(fecha_ref_str)),
    fecha_ref = if_else(is.na(fecha_ref),
                        suppressWarnings(dmy(fecha_ref_str)),
                        fecha_ref)
  ) |>
  # 5) Normalizo campos comunes que usa el tablero
  mutate(
    # Zonas vacías o NA pasan a "Sin zona"
    ID_zona   = if_else(is.na(ID_zona) | ID_zona == "", "Sin zona", ID_zona),
    # Destino vacío o NA pasa a "Sin destino"
    De_nombre = if_else(is.na(De_nombre) | De_nombre == "", "Sin destino", De_nombre)
  )

# 6) Valores auxiliares para los filtros del tablero
fecha_min     <- min(agencia_base$fecha_ref, na.rm = TRUE)
fecha_max     <- max(agencia_base$fecha_ref, na.rm = TRUE)
zonas_choices <- sort(unique(agencia_base$ID_zona))