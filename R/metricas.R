# FUNCIONES DE MÉTRICAS

# La idea: el tablero llama a estas funciones
# pasándoles la tabla ya filtrada.

library(dplyr)
library(lubridate)

# --- KPI---
kpi_ventas_tot <- function(df) {
  sum(df$venta_total_ml, na.rm = TRUE)
}

kpi_renta_tot <- function(df) {
  sum(df$renta_ml, na.rm = TRUE)
}

kpi_clientes_unicos <- function(df) {
  dplyr::n_distinct(df$ID_cliente)
}

# --- Serie mensual de ventas  ---
serie_mensual <- function(df) {
  df |>
    filter(!is.na(fecha_ref)) |>
    mutate(mes = floor_date(fecha_ref, "month")) |>
    group_by(mes) |>
    summarise(ventas = sum(venta_total_ml, na.rm = TRUE), .groups = "drop")
}

# --- Top 10 vendedores (para la tabla) ---
top_vendedores <- function(df, n = 10) {
  df |>
    group_by(Vendedor) |>
    summarise(ventas = sum(venta_total_ml, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(ventas)) |>
    slice_head(n = n)
}

# --- Ventas por zona con Top 9 + otros (para la torta) ---
ventas_por_zona_top10 <- function(df) {
  res <- df |>
    group_by(ID_zona) |>
    summarise(ventas = sum(venta_total_ml, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(ventas))
  if (nrow(res) > 10) {
    top9  <- slice(res, 1:9)
    otros <- data.frame(ID_zona = "Otros",
                        ventas  = sum(res$ventas[-seq_len(9)], na.rm = TRUE))
    res <- bind_rows(top9, otros)
  }
  res
}

# --- Top 10 destinos por pasajeros (PAX) ---
top_destinos_pax <- function(df, n = 10) {
  df |>
    group_by(De_nombre) |>
    summarise(paxs = sum(cantidad_de_paxs, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(paxs)) |>
    slice_head(n = n)
}