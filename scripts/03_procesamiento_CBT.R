# ============================================================
# Procesamiento de CBT
# Proyecto grupal MVD 2026
# ============================================================

# Objetivo:
# leer las series oficiales de CBA y CBT de INDEC, leer IPC-CIFRA,
# construir CBT trimestral y definir la CBT final usada en el proyecto.

# MODIFICACIÓN:
# para 2007, 2008, 2010 y 2011 no se usa directamente la CBT oficial de INDEC.
# Se reconstruye una CBT alternativa actualizando la CBT base 2007T1 con IPC-CIFRA.
# Para 2018, 2019, 2021 y 2022 se mantiene la CBT oficial de INDEC.

library(tidyverse)
library(readxl)
library(readr)
library(here)
library(lubridate)

message("Procesando CBT final del proyecto...")

# ============================================================
# 1. Rutas de archivos
# ============================================================

path_cbt_historica <- here("data_raw", "externas", "sh-cba2.xls")
path_cbt_reciente <- here("data_raw", "externas", "serie_cba_cbt.xls")

# MODIFICACION:
# se incorpora el IPC-CIFRA como fuente alternativa para corregir el primer bloque.
path_ipc_cifra <- here("data_raw", "externas", "IPC-Provincias-2007-2018.xlsx")

# ============================================================
# 2. Periodos del proyecto
# ============================================================

periodos_proyecto <- expand_grid(
  anio = c(2007, 2008, 2010, 2011, 2018, 2019, 2021, 2022),
  trimestre = 1:4
) |>
  filter(!(anio == 2007 & trimestre == 3)) |>
  arrange(anio, trimestre) |>
  mutate(
    periodo = paste0(anio, "_t", trimestre),
    bloque = case_when(
      anio %in% c(2007, 2008, 2010, 2011) ~ "2007-2011",
      anio %in% c(2018, 2019, 2021, 2022) ~ "2018-2022",
      TRUE ~ NA_character_
    )
  )

periodos_cifra <- c(2007, 2008, 2010, 2011)
periodos_indec <- c(2018, 2019, 2021, 2022)

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
  
  fecha_chr[fecha_chr %in% c("", "na", "nan", "s/d", "-", "--")] <- NA_character_
  
  resultado <- rep(as.Date(NA), length(fecha_chr))
  
  # Detecta fechas que vienen como numero serial de Excel.
  # MODIFICACION:
  # se fuerza que los NA de str_detect sean FALSE para que no rompan el indexado.
  es_serial_excel <- str_detect(fecha_chr, "^\\d+$")
  es_serial_excel[is.na(es_serial_excel)] <- FALSE
  
  if (any(es_serial_excel)) {
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
        "my",
        "b-y",
        "b-Y",
        "B-y",
        "B-Y"
      ),
      locale = "es_ES"
    )
  )
  
  reemplazar_parseadas <- is.na(resultado) & !is.na(fechas_parseadas)
  reemplazar_parseadas[is.na(reemplazar_parseadas)] <- FALSE
  
  if (any(reemplazar_parseadas)) {
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
      "sep", "set", "sept", "septiembre", "setiembre",
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
      9, 9, 9, 9, 9,
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
    
    if (is.na(texto)) {
      next
    }
    
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
      fuente_indec = fuente
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
      fuente_indec
    ) |>
    arrange(anio, mes)
}

# MODIFICACION:
# esta funcion lee el Excel de IPC-CIFRA, que viene con una fila de titulo,
# una fila de encabezados y luego la serie mensual.

leer_ipc_cifra <- function(path) {
  
  # MODIFICACION:
  # el archivo de IPC-CIFRA tiene titulo en fila 1, encabezados en fila 3
  # y datos desde la fila 4. La columna Periodo esta guardada como fecha serial de Excel.
  
  base_raw <- read_excel(
    path,
    sheet = "Hoja1",
    skip = 3,
    col_names = c("periodo_raw", "ipc_cifra_raw"),
    col_types = c("text", "numeric")
  ) |>
    select(
      periodo_raw,
      ipc_cifra_raw
    )
  
  base_raw |>
    mutate(
      periodo_chr = as.character(periodo_raw) |>
        str_trim() |>
        str_to_lower(),
      
      periodo_num = suppressWarnings(as.numeric(periodo_chr)),
      
      fecha_serial = case_when(
        !is.na(periodo_num) ~ as.Date(periodo_num, origin = "1899-12-30"),
        TRUE ~ as.Date(NA)
      ),
      
      mes_texto = str_extract(periodo_chr, "^[a-záéíóúñ]+"),
      anio_texto = str_extract(periodo_chr, "\\d{2,4}$"),
      
      mes_manual = case_when(
        mes_texto %in% c("ene", "enero") ~ 1,
        mes_texto %in% c("feb", "febrero") ~ 2,
        mes_texto %in% c("mar", "marzo") ~ 3,
        mes_texto %in% c("abr", "abril") ~ 4,
        mes_texto %in% c("may", "mayo") ~ 5,
        mes_texto %in% c("jun", "junio") ~ 6,
        mes_texto %in% c("jul", "julio") ~ 7,
        mes_texto %in% c("ago", "agosto") ~ 8,
        mes_texto %in% c("sep", "set", "sept", "septiembre", "setiembre") ~ 9,
        mes_texto %in% c("oct", "octubre") ~ 10,
        mes_texto %in% c("nov", "noviembre") ~ 11,
        mes_texto %in% c("dic", "diciembre") ~ 12,
        TRUE ~ NA_real_
      ),
      
      anio_manual = suppressWarnings(as.numeric(anio_texto)),
      anio_manual = case_when(
        !is.na(anio_manual) & anio_manual < 100 & anio_manual <= 50 ~ 2000 + anio_manual,
        !is.na(anio_manual) & anio_manual < 100 & anio_manual > 50 ~ 1900 + anio_manual,
        TRUE ~ anio_manual
      ),
      
      fecha_manual_chr = case_when(
        !is.na(mes_manual) & !is.na(anio_manual) ~ paste0(
          anio_manual,
          "-",
          str_pad(mes_manual, 2, pad = "0"),
          "-01"
        ),
        TRUE ~ NA_character_
      ),
      
      fecha_manual = suppressWarnings(ymd(fecha_manual_chr)),
      
      fecha = coalesce(fecha_serial, fecha_manual),
      ipc_cifra = ipc_cifra_raw
    ) |>
    filter(!is.na(fecha)) |>
    filter(!is.na(ipc_cifra)) |>
    mutate(
      fecha = floor_date(fecha, unit = "month"),
      anio = year(fecha),
      mes = month(fecha),
      trimestre = quarter(fecha),
      fuente_ipc = "IPC-CIFRA Provincias"
    ) |>
    select(
      anio,
      trimestre,
      mes,
      fecha,
      ipc_cifra,
      fuente_ipc
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

# MODIFICACION:
# se lee IPC-CIFRA como indice mensual, no como variacion mensual.
ipc_cifra_mensual <- leer_ipc_cifra(path_ipc_cifra)

# ============================================================
# 5. Unificacion de CBT INDEC mensual
# ============================================================

cbt_mensual_indec <- bind_rows(
  cbt_historica,
  cbt_reciente
) |>
  arrange(anio, mes, fuente_indec) |>
  distinct(anio, mes, .keep_all = TRUE)

# ============================================================
# 6. Trimestralizacion de series
# ============================================================

cbt_trimestral_indec <- cbt_mensual_indec |>
  group_by(anio, trimestre) |>
  summarise(
    meses_indec = n(),
    cba_indec_adulto_equiv = mean(cba_adulto_equiv, na.rm = TRUE),
    cbt_indec_adulto_equiv = mean(cbt_adulto_equiv, na.rm = TRUE),
    fuente_indec = paste(unique(fuente_indec), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

# MODIFICACION:
# la EPH es trimestral, por eso IPC-CIFRA se pasa de mensual a trimestral
# con el promedio simple de los tres meses del trimestre.

ipc_cifra_trimestral <- ipc_cifra_mensual |>
  group_by(anio, trimestre) |>
  summarise(
    meses_cifra = n(),
    ipc_cifra_trimestral = mean(ipc_cifra, na.rm = TRUE),
    fuente_ipc = paste(unique(fuente_ipc), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

# ============================================================
# 7. Reconstruccion de CBT con IPC-CIFRA
# ============================================================

# MODIFICACION:
# se toma 2007T1 como trimestre base.
# La CBT base sale de la serie INDEC y luego se actualiza con IPC-CIFRA.

cbt_base_2007_t1 <- cbt_trimestral_indec |>
  filter(anio == 2007, trimestre == 1) |>
  pull(cbt_indec_adulto_equiv)

ipc_base_2007_t1 <- ipc_cifra_trimestral |>
  filter(anio == 2007, trimestre == 1) |>
  pull(ipc_cifra_trimestral)

if (length(cbt_base_2007_t1) == 0 || length(cbt_base_2007_t1) != 1 || is.na(cbt_base_2007_t1)) {
  stop("No se pudo definir la CBT base 2007T1 desde INDEC.", call. = FALSE)
}

if (length(ipc_base_2007_t1) == 0 || length(ipc_base_2007_t1) != 1 || is.na(ipc_base_2007_t1)) {
  stop("No se pudo definir el IPC-CIFRA base 2007T1.", call. = FALSE)
}

cbt_cifra_trimestral <- ipc_cifra_trimestral |>
  mutate(
    cbt_base_2007_t1 = cbt_base_2007_t1,
    ipc_base_2007_t1 = ipc_base_2007_t1,
    cbt_cifra_adulto_equiv = cbt_base_2007_t1 *
      ipc_cifra_trimestral / ipc_base_2007_t1
  ) |>
  select(
    anio,
    trimestre,
    meses_cifra,
    ipc_cifra_trimestral,
    ipc_base_2007_t1,
    cbt_base_2007_t1,
    cbt_cifra_adulto_equiv,
    fuente_ipc
  )

# ============================================================
# 8. CBT final del proyecto
# ============================================================

# MODIFICACION:
# se crea una variable unica de CBT final.
# Esta es la que debe usar el script 04 para clasificar pobreza.

cbt_trimestral_proyecto <- periodos_proyecto |>
  left_join(
    cbt_trimestral_indec,
    by = c("anio", "trimestre")
  ) |>
  left_join(
    cbt_cifra_trimestral,
    by = c("anio", "trimestre")
  ) |>
  mutate(
    cbt_final_adulto_equiv = case_when(
      anio %in% periodos_cifra ~ cbt_cifra_adulto_equiv,
      anio %in% periodos_indec ~ cbt_indec_adulto_equiv,
      TRUE ~ NA_real_
    ),
    fuente_cbt = case_when(
      anio %in% periodos_cifra ~ "CBT reconstruida con IPC-CIFRA",
      anio %in% periodos_indec ~ "CBT oficial INDEC",
      TRUE ~ NA_character_
    ),
    bloque_confiabilidad = case_when(
      anio %in% periodos_cifra ~ "Primer bloque exploratorio",
      anio %in% periodos_indec ~ "Segundo bloque principal",
      TRUE ~ NA_character_
    ),
    cbt_final_disponible = case_when(
      !is.na(cbt_final_adulto_equiv) ~ 1,
      TRUE ~ 0
    ),
    trimestre_completo_indec = case_when(
      meses_indec == 3 ~ 1,
      TRUE ~ 0
    ),
    trimestre_completo_cifra = case_when(
      meses_cifra == 3 ~ 1,
      TRUE ~ 0
    ),
    trimestre_completo_fuente_usada = case_when(
      anio %in% periodos_cifra & trimestre_completo_cifra == 1 ~ 1,
      anio %in% periodos_indec & trimestre_completo_indec == 1 ~ 1,
      TRUE ~ 0
    )
  ) |>
  select(
    anio,
    trimestre,
    periodo,
    bloque,
    cba_indec_adulto_equiv,
    cbt_indec_adulto_equiv,
    cbt_cifra_adulto_equiv,
    cbt_final_adulto_equiv,
    fuente_cbt,
    bloque_confiabilidad,
    ipc_cifra_trimestral,
    cbt_base_2007_t1,
    ipc_base_2007_t1,
    meses_indec,
    meses_cifra,
    trimestre_completo_indec,
    trimestre_completo_cifra,
    trimestre_completo_fuente_usada,
    cbt_final_disponible,
    fuente_indec,
    fuente_ipc
  ) |>
  arrange(anio, trimestre)

# ============================================================
# 9. Chequeo general
# ============================================================

periodos_sin_cbt_final <- cbt_trimestral_proyecto |>
  filter(cbt_final_disponible == 0)

periodos_fuente_incompleta <- cbt_trimestral_proyecto |>
  filter(trimestre_completo_fuente_usada == 0)

chequeo_cbt <- tibble(
  periodos_esperados = nrow(periodos_proyecto),
  periodos_con_cbt_final = sum(cbt_trimestral_proyecto$cbt_final_disponible == 1, na.rm = TRUE),
  periodos_sin_cbt_final = nrow(periodos_sin_cbt_final),
  periodos_con_fuente_incompleta = nrow(periodos_fuente_incompleta),
  cbt_base_2007_t1 = cbt_base_2007_t1,
  ipc_base_2007_t1 = ipc_base_2007_t1,
  meses_indec_totales = nrow(cbt_mensual_indec),
  meses_cifra_totales = nrow(ipc_cifra_mensual),
  fecha_minima_indec = min(cbt_mensual_indec$fecha, na.rm = TRUE),
  fecha_maxima_indec = max(cbt_mensual_indec$fecha, na.rm = TRUE),
  fecha_minima_cifra = min(ipc_cifra_mensual$fecha, na.rm = TRUE),
  fecha_maxima_cifra = max(ipc_cifra_mensual$fecha, na.rm = TRUE)
)

print(chequeo_cbt)
print(cbt_trimestral_proyecto, n = Inf)

if (nrow(periodos_sin_cbt_final) > 0) {
  print(periodos_sin_cbt_final, n = Inf)
  stop("Faltan periodos con CBT final. Revisar fuentes.", call. = FALSE)
}

if (nrow(periodos_fuente_incompleta) > 0) {
  print(periodos_fuente_incompleta, n = Inf)
  stop("Hay trimestres incompletos en la fuente usada. Revisar INDEC o IPC-CIFRA.", call. = FALSE)
}

# ============================================================
# 10. Guardado de resultados
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
  ipc_cifra_mensual,
  here("data_processed", "ipc_cifra_mensual.rds")
)

write_rds(
  ipc_cifra_trimestral,
  here("data_processed", "ipc_cifra_trimestral.rds")
)

write_rds(
  cbt_cifra_trimestral,
  here("data_processed", "cbt_cifra_trimestral.rds")
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
  ipc_cifra_mensual,
  here("output", "tablas", "ipc_cifra_mensual.csv")
)

write_csv(
  ipc_cifra_trimestral,
  here("output", "tablas", "ipc_cifra_trimestral.csv")
)

write_csv(
  cbt_cifra_trimestral,
  here("output", "tablas", "cbt_cifra_trimestral.csv")
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
  periodos_sin_cbt_final,
  here("output", "tablas", "periodos_sin_cbt_final.csv")
)

write_csv(
  periodos_fuente_incompleta,
  here("output", "tablas", "periodos_fuente_incompleta_cbt.csv")
)

message("Procesamiento de CBT final del proyecto terminado.")