# ============================================================
# Procesamiento de CBT
# Proyecto grupal MVD 2026
# ============================================================

# Objetivo:
# leer dos series oficiales de CBA y CBT de INDEC, limpiarlas,
# unificarlas y construir una CBT trimestral por adulto equivalente
# para los periodos utilizados en el proyecto.

library(tidyverse)
library(readxl)
library(readr)
library(here)
library(lubridate)

message("Procesando CBT de INDEC...")

# ============================================================
# 1. Rutas de archivos
# ============================================================

path_cbt_historica <- here("data_raw", "externas", "sh-cba2.xls")
path_cbt_reciente <- here("data_raw", "externas", "serie_cba_cbt.xls")

# ============================================================
# 2. Periodos del proyecto
# ============================================================

periodos_proyecto <- expand_grid(
  anio = c(2007, 2008, 2010, 2011, 2018, 2019, 2021, 2022),
  trimestre = 1:4
) |>
  filter(!(anio == 2007 & trimestre == 3)) |>
  arrange(anio, trimestre)

# ============================================================
# 3. Funciones auxiliares
# ============================================================

convertir_numero_cbt <- function(x) {
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  x_chr <- as.character(x) |>
    str_trim()
  
  x_chr[x_chr %in% c("", "NA", "NaN", "///", "-", "--", ".", "s/d", "S/D")] <- NA_character_
  
  tiene_coma <- str_detect(x_chr, ",")
  tiene_coma[is.na(tiene_coma)] <- FALSE
  
  resultado <- rep(NA_real_, length(x_chr))
  
  posiciones_con_coma <- which(tiene_coma & !is.na(x_chr))
  posiciones_sin_coma <- which(!tiene_coma & !is.na(x_chr))
  
  if (length(posiciones_con_coma) > 0) {
    resultado[posiciones_con_coma] <- parse_number(
      x_chr[posiciones_con_coma],
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    )
  }
  
  if (length(posiciones_sin_coma) > 0) {
    resultado[posiciones_sin_coma] <- parse_number(
      x_chr[posiciones_sin_coma],
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
  }
  
  resultado
}

convertir_fecha_cbt <- function(fecha_raw) {
  
  if (inherits(fecha_raw, "Date") |
      inherits(fecha_raw, "POSIXct") |
      inherits(fecha_raw, "POSIXt")) {
    return(as.Date(fecha_raw))
  }
  
  fecha_chr <- as.character(fecha_raw) |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("\\.", "") |>
    str_replace_all("/", "-") |>
    str_replace_all("_", "-")
  
  resultado <- rep(as.Date(NA), length(fecha_chr))
  
  es_serial_excel <- str_detect(fecha_chr, "^\\d+$")
  
  if (any(es_serial_excel, na.rm = TRUE)) {
    resultado[es_serial_excel] <- as.Date(
      as.numeric(fecha_chr[es_serial_excel]),
      origin = "1899-12-30"
    )
  }
  
  fechas_parseadas <- suppressWarnings(
    parse_date_time(
      fecha_chr,
      orders = c(
        "ymd HMS",
        "ymd",
        "dmy HMS",
        "dmy",
        "mdy HMS",
        "mdy",
        "my"
      )
    )
  )
  
  reemplazar_parseadas <- is.na(resultado) & !is.na(fechas_parseadas)
  
  if (any(reemplazar_parseadas, na.rm = TRUE)) {
    resultado[reemplazar_parseadas] <- as.Date(fechas_parseadas[reemplazar_parseadas])
  }
  
  meses <- tibble(
    mes_texto = c(
      "ene", "enero",
      "feb", "febrero",
      "mar", "marzo",
      "abr", "abril",
      "may", "mayo",
      "jun", "junio",
      "jul", "julio",
      "ago", "agosto",
      "sep", "set", "septiembre", "setiembre",
      "oct", "octubre",
      "nov", "noviembre",
      "dic", "diciembre"
    ),
    mes_numero = c(
      1, 1,
      2, 2,
      3, 3,
      4, 4,
      5, 5,
      6, 6,
      7, 7,
      8, 8,
      9, 9, 9, 9,
      10, 10,
      11, 11,
      12, 12
    )
  )
  
  for (i in seq_along(fecha_chr)) {
    
    if (!is.na(resultado[i])) {
      next
    }
    
    texto <- fecha_chr[i]
    
    mes_detectado <- meses |>
      filter(str_detect(texto, mes_texto)) |>
      slice(1) |>
      pull(mes_numero)
    
    anio_detectado <- str_extract(texto, "\\d{4}|\\d{2}$")
    
    if (length(mes_detectado) == 1 && !is.na(anio_detectado)) {
      
      anio_numero <- as.numeric(anio_detectado)
      
      if (anio_numero < 100) {
        anio_numero <- if_else(
          anio_numero <= 50,
          2000 + anio_numero,
          1900 + anio_numero
        )
      }
      
      resultado[i] <- as.Date(
        paste0(anio_numero, "-", str_pad(mes_detectado, 2, pad = "0"), "-01")
      )
    }
  }
  
  resultado
}

leer_cbt_indec <- function(path, sheet, skip, fuente) {
  
  read_excel(
    path,
    sheet = sheet,
    skip = skip,
    col_names = FALSE
  ) |>
    select(
      fecha = 1,
      cba_adulto_equiv = 2,
      inversa_engel = 3,
      cbt_adulto_equiv = 4
    ) |>
    filter(!is.na(fecha)) |>
    mutate(
      fecha = convertir_fecha_cbt(fecha),
      cba_adulto_equiv = convertir_numero_cbt(cba_adulto_equiv),
      inversa_engel = convertir_numero_cbt(inversa_engel),
      cbt_adulto_equiv = convertir_numero_cbt(cbt_adulto_equiv),
      fuente = fuente
    ) |>
    filter(!is.na(fecha)) |>
    filter(!is.na(cbt_adulto_equiv)) |>
    mutate(
      anio = year(fecha),
      mes = month(fecha),
      trimestre = quarter(fecha)
    ) |>
    select(
      anio,
      trimestre,
      mes,
      fecha,
      cba_adulto_equiv,
      inversa_engel,
      cbt_adulto_equiv,
      fuente
    ) |>
    arrange(anio, mes)
}

# ============================================================
# 4. Lectura de series
# ============================================================

cbt_historica <- leer_cbt_indec(
  path = path_cbt_historica,
  sheet = "CBA y CBT",
  skip = 9,
  fuente = "INDEC historica"
)

cbt_reciente <- leer_cbt_indec(
  path = path_cbt_reciente,
  sheet = "CBA-CBT",
  skip = 7,
  fuente = "INDEC serie reciente"
)

# ============================================================
# 5. Unificacion de series
# ============================================================

cbt_mensual_indec <- bind_rows(
  cbt_historica,
  cbt_reciente
) |>
  arrange(anio, mes, fuente) |>
  distinct(anio, mes, .keep_all = TRUE)

# ============================================================
# 6. Construccion de CBT trimestral
# ============================================================

cbt_trimestral_indec <- cbt_mensual_indec |>
  group_by(anio, trimestre) |>
  summarise(
    meses_disponibles = n(),
    cba_adulto_equiv = mean(cba_adulto_equiv, na.rm = TRUE),
    cbt_adulto_equiv = mean(cbt_adulto_equiv, na.rm = TRUE),
    fuente = paste(unique(fuente), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

cbt_trimestral_proyecto <- periodos_proyecto |>
  left_join(
    cbt_trimestral_indec,
    by = c("anio", "trimestre")
  ) |>
  mutate(
    cbt_disponible = case_when(
      !is.na(cbt_adulto_equiv) ~ 1,
      TRUE ~ 0
    ),
    trimestre_completo = case_when(
      meses_disponibles == 3 ~ 1,
      TRUE ~ 0
    )
  )

periodos_sin_cbt <- cbt_trimestral_proyecto |>
  filter(cbt_disponible == 0)

periodos_incompletos <- cbt_trimestral_proyecto |>
  filter(cbt_disponible == 1, trimestre_completo == 0)

# ============================================================
# 7. Chequeos
# ============================================================

chequeo_cbt <- tibble(
  periodos_esperados = nrow(periodos_proyecto),
  periodos_con_cbt = sum(cbt_trimestral_proyecto$cbt_disponible == 1, na.rm = TRUE),
  periodos_sin_cbt = nrow(periodos_sin_cbt),
  periodos_incompletos = nrow(periodos_incompletos),
  fecha_minima = min(cbt_mensual_indec$fecha, na.rm = TRUE),
  fecha_maxima = max(cbt_mensual_indec$fecha, na.rm = TRUE),
  meses_totales = nrow(cbt_mensual_indec)
)

print(chequeo_cbt)
print(periodos_sin_cbt, n = Inf)
print(periodos_incompletos, n = Inf)
print(cbt_trimestral_proyecto, n = Inf)

if (nrow(periodos_sin_cbt) > 0) {
  stop("Faltan periodos con CBT. Revisar fuentes de INDEC.")
}

if (nrow(periodos_incompletos) > 0) {
  stop("Hay trimestres con menos de tres meses de CBT. Revisar fuentes de INDEC.")
}

# ============================================================
# 8. Guardado de resultados
# ============================================================

write_rds(
  cbt_mensual_indec,
  here("data_processed", "cbt_mensual_indec.rds")
)

write_rds(
  cbt_trimestral_indec,
  here("data_processed", "cbt_trimestral_indec.rds")
)

write_rds(
  cbt_trimestral_proyecto,
  here("data_processed", "cbt_trimestral_proyecto.rds")
)

write_csv(
  cbt_mensual_indec,
  here("output", "tablas", "cbt_mensual_indec.csv")
)

write_csv(
  cbt_trimestral_indec,
  here("output", "tablas", "cbt_trimestral_indec.csv")
)

write_csv(
  cbt_trimestral_proyecto,
  here("output", "tablas", "cbt_trimestral_proyecto.csv")
)

write_csv(
  chequeo_cbt,
  here("output", "tablas", "chequeo_cbt.csv")
)

write_csv(
  periodos_sin_cbt,
  here("output", "tablas", "periodos_sin_cbt.csv")
)

write_csv(
  periodos_incompletos,
  here("output", "tablas", "periodos_incompletos_cbt.csv")
)

message("Procesamiento de CBT de INDEC finalizado.")