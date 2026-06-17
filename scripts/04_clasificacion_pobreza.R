# ============================================================
# Clasificacion de pobreza por ingresos
# Proyecto final MVD 2026
# ============================================================

# Objetivo:
# unir hogares EPH, composicion del hogar y CBT final para clasificar pobreza
# en CABA y Partidos del GBA.

# MODIFICACION:
# ahora la pobreza se calcula con cbt_final_adulto_equiv, construida en el script 03.
# Para 2007-2011 usa CBT reconstruida con IPC-CIFRA y para 2018-2022 usa CBT oficial INDEC.

library(tidyverse)
library(readr)
library(here)

message("Clasificando pobreza por ingresos...")

dir.create(here("output", "tablas"), recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. Lectura de bases procesadas
# ============================================================

hogares_eph_gba <- read_rds(
  here("data_processed", "hogares_eph_gba.rds")
)

adulto_equiv_hogar <- read_rds(
  here("data_processed", "adulto_equiv_hogar.rds")
)

cbt_trimestral_proyecto <- read_rds(
  here("data_processed", "cbt_trimestral_proyecto.rds")
)

# ============================================================
# 2. Funciones auxiliares
# ============================================================

media_ponderada <- function(x, w) {
  
  ok <- !is.na(x) & !is.na(w) & w > 0
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  sum(x[ok] * w[ok]) / sum(w[ok])
}

mediana_na <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  median(x, na.rm = TRUE)
}

media_na <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}

dividir_seguro <- function(numerador, denominador) {
  
  case_when(
    !is.na(denominador) & denominador > 0 ~ numerador / denominador,
    TRUE ~ NA_real_
  )
}

# ============================================================
# 3. Chequeos minimos antes de unir
# ============================================================

variables_hogares <- c(
  "id_hogar",
  "anio",
  "trimestre",
  "periodo",
  "gba",
  "itf",
  "ipcf",
  "pondera",
  "pondih",
  "ponderador_ingreso",
  "no_respuesta_ingresos",
  "ingreso_cero"
)

variables_composicion <- c(
  "id_hogar",
  "miembros_hogar",
  "n_integrantes",
  "adulto_equiv_hogar",
  "personas_sin_adulto_equiv",
  "adulto_equiv_valido",
  "n_menores_18",
  "prop_menores",
  "n_adultos_18_64",
  "n_mayores_65",
  "n_dependientes",
  "tasa_dependencia_hogar",
  "menores_por_adulto_activo",
  "adulto_equiv_por_adulto_activo",
  "tipo_estructura_etaria",
  "carga_infantil",
  "menores_un_adulto_activo",
  "hogar_numeroso",
  "n_ocupados",
  "tipo_insercion_laboral",
  "n_perceptores_ingreso",
  "tipo_sosten_ingresos",
  "perceptores_por_integrante",
  "adulto_equiv_por_perceptor",
  "adulto_equiv_por_ocupado",
  "dependientes_por_perceptor",
  "menores_por_perceptor",
  "sexo_jefe",
  "edad_jefe",
  "estado_jefe",
  "condicion_actividad_jefe",
  "cat_ocup_jefe",
  "jefe_ocupado"
)

variables_cbt <- c(
  "anio",
  "trimestre",
  "periodo",
  "cbt_indec_adulto_equiv",
  "cbt_cifra_adulto_equiv",
  "cbt_final_adulto_equiv",
  "fuente_cbt",
  "bloque_confiabilidad"
)

faltan_hogares <- setdiff(variables_hogares, names(hogares_eph_gba))
faltan_composicion <- setdiff(variables_composicion, names(adulto_equiv_hogar))
faltan_cbt <- setdiff(variables_cbt, names(cbt_trimestral_proyecto))

if (length(faltan_hogares) > 0) {
  stop(
    "Faltan variables indispensables en hogares_eph_gba: ",
    paste(faltan_hogares, collapse = ", "),
    call. = FALSE
  )
}

if (length(faltan_composicion) > 0) {
  stop(
    "Faltan variables indispensables en adulto_equiv_hogar: ",
    paste(faltan_composicion, collapse = ", "),
    call. = FALSE
  )
}

if (length(faltan_cbt) > 0) {
  stop(
    "Faltan variables indispensables en cbt_trimestral_proyecto: ",
    paste(faltan_cbt, collapse = ", "),
    call. = FALSE
  )
}

ids_duplicados_composicion <- adulto_equiv_hogar |>
  count(id_hogar) |>
  filter(n > 1)

periodos_duplicados_cbt <- cbt_trimestral_proyecto |>
  count(anio, trimestre, periodo) |>
  filter(n > 1)

if (nrow(ids_duplicados_composicion) > 0) {
  stop("Hay id_hogar duplicados en adulto_equiv_hogar. Revisar script 02.", call. = FALSE)
}

if (nrow(periodos_duplicados_cbt) > 0) {
  stop("Hay periodos duplicados en cbt_trimestral_proyecto. Revisar script 03.", call. = FALSE)
}

# ============================================================
# 4. Union de bases
# ============================================================

# MODIFICACION:
# se une la base de hogares con todas las variables de composicion creadas en el script 02.
# Tambien se une la CBT final creada en el script 03.

composicion_para_join <- adulto_equiv_hogar |>
  select(
    all_of(variables_composicion)
  )

cbt_para_join <- cbt_trimestral_proyecto |>
  select(
    all_of(variables_cbt)
  )

hogares_con_cbt <- hogares_eph_gba |>
  left_join(
    composicion_para_join,
    by = "id_hogar"
  ) |>
  left_join(
    cbt_para_join,
    by = c("anio", "trimestre", "periodo")
  )

if (nrow(hogares_con_cbt) != nrow(hogares_eph_gba)) {
  stop("La union modifico la cantidad de hogares. Revisar claves de union.", call. = FALSE)
}

# ============================================================
# 5. Chequeo general de union
# ============================================================

chequeo_union_pobreza <- hogares_con_cbt |>
  group_by(anio, trimestre, periodo, gba) |>
  summarise(
    hogares = n(),
    hogares_sin_adulto_equiv = sum(is.na(adulto_equiv_hogar)),
    hogares_adulto_equiv_invalido = sum(adulto_equiv_valido == 0, na.rm = TRUE),
    hogares_sin_cbt_final = sum(is.na(cbt_final_adulto_equiv)),
    hogares_sin_respuesta_ingresos = sum(no_respuesta_ingresos == 1, na.rm = TRUE),
    hogares_ingreso_cero = sum(ingreso_cero == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre, gba)

print(chequeo_union_pobreza, n = Inf)

if (any(chequeo_union_pobreza$hogares_sin_adulto_equiv > 0, na.rm = TRUE)) {
  stop("Hay hogares sin adulto equivalente. Revisar script 02.", call. = FALSE)
}

if (any(chequeo_union_pobreza$hogares_adulto_equiv_invalido > 0, na.rm = TRUE)) {
  stop("Hay hogares con adulto equivalente invalido. Revisar script 02.", call. = FALSE)
}

if (any(chequeo_union_pobreza$hogares_sin_cbt_final > 0, na.rm = TRUE)) {
  stop("Hay hogares sin CBT final. Revisar script 03.", call. = FALSE)
}

# ============================================================
# 6. Clasificacion de pobreza
# ============================================================

# MODIFICACION:
# la linea de pobreza del hogar se calcula con la CBT final por adulto equivalente.
# La CBT final ya trae resuelta la fuente que corresponde a cada periodo.

hogares_pobreza <- hogares_con_cbt |>
  mutate(
    entra_medicion_pobreza = case_when(
      no_respuesta_ingresos == 0 &
        !is.na(itf) &
        itf >= 0 &
        adulto_equiv_valido == 1 &
        !is.na(adulto_equiv_hogar) &
        adulto_equiv_hogar > 0 &
        !is.na(cbt_final_adulto_equiv) ~ 1,
      TRUE ~ 0
    ),
    linea_pobreza_hogar = case_when(
      entra_medicion_pobreza == 1 ~ cbt_final_adulto_equiv * adulto_equiv_hogar,
      TRUE ~ NA_real_
    ),
    pobre = case_when(
      entra_medicion_pobreza == 1 & itf < linea_pobreza_hogar ~ 1,
      entra_medicion_pobreza == 1 & itf >= linea_pobreza_hogar ~ 0,
      TRUE ~ NA_real_
    ),
    situacion_pobreza = case_when(
      pobre == 1 ~ "Pobre",
      pobre == 0 ~ "No pobre",
      TRUE ~ "Fuera de medicion"
    ),
    situacion_pobreza = factor(
      situacion_pobreza,
      levels = c("Pobre", "No pobre", "Fuera de medicion")
    )
  )

# ============================================================
# 7. Variables de intensidad y vulnerabilidad
# ============================================================

# MODIFICACION:
# se agregan medidas de intensidad de pobreza y distancia a la linea.
# Estas variables permiten analizar no solo si el hogar es pobre,
# sino cuan lejos esta de superar la linea.

hogares_pobreza <- hogares_pobreza |>
  mutate(
    ratio_itf_linea = case_when(
      entra_medicion_pobreza == 1 ~ itf / linea_pobreza_hogar,
      TRUE ~ NA_real_
    ),
    ratio_itf_cbt = ratio_itf_linea,
    distancia_linea = case_when(
      entra_medicion_pobreza == 1 ~ ratio_itf_linea - 1,
      TRUE ~ NA_real_
    ),
    brecha_pobreza_abs = case_when(
      pobre == 1 ~ linea_pobreza_hogar - itf,
      TRUE ~ NA_real_
    ),
    brecha_pobreza_rel = case_when(
      pobre == 1 ~ (linea_pobreza_hogar - itf) / linea_pobreza_hogar,
      TRUE ~ NA_real_
    ),
    ingreso_adulto_equiv = dividir_seguro(itf, adulto_equiv_hogar),
    ingreso_por_perceptor = dividir_seguro(itf, n_perceptores_ingreso),
    cerca_linea_pobreza = case_when(
      entra_medicion_pobreza == 1 & pobre == 0 & ratio_itf_linea < 1.25 ~ 1,
      entra_medicion_pobreza == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# ============================================================
# 8. Variables temporales de crisis
# ============================================================

# MODIFICACION:
# se crean variables temporales para comparar antes y despues de cada crisis.

hogares_pobreza <- hogares_pobreza |>
  mutate(
    crisis = case_when(
      anio %in% c(2007, 2008, 2010, 2011) ~ "Crisis 2009",
      anio %in% c(2018, 2019, 2021, 2022) ~ "Crisis 2020",
      TRUE ~ NA_character_
    ),
    momento = case_when(
      anio %in% c(2007, 2008, 2018, 2019) ~ "Antes",
      anio %in% c(2010, 2011, 2021, 2022) ~ "Despues",
      TRUE ~ NA_character_
    ),
    periodo_crisis = case_when(
      crisis == "Crisis 2009" & momento == "Antes" ~ "Antes de crisis 2009",
      crisis == "Crisis 2009" & momento == "Despues" ~ "Despues de crisis 2009",
      crisis == "Crisis 2020" & momento == "Antes" ~ "Antes de crisis 2020",
      crisis == "Crisis 2020" & momento == "Despues" ~ "Despues de crisis 2020",
      TRUE ~ NA_character_
    ),
    etapa_crisis = periodo_crisis,
    bloque = case_when(
      anio %in% c(2007, 2008, 2010, 2011) ~ "2007-2011",
      anio %in% c(2018, 2019, 2021, 2022) ~ "2018-2022",
      TRUE ~ NA_character_
    ),
    post_crisis = case_when(
      momento == "Despues" ~ 1,
      momento == "Antes" ~ 0,
      TRUE ~ NA_real_
    )
  )

# ============================================================
# 9. Variables de composicion asociadas a pobreza
# ============================================================

# MODIFICACION:
# ahora si se crean variables como hogar ocupado pobre y jefe ocupado pobre,
# porque recien en este script existe la variable pobre.

hogares_pobreza <- hogares_pobreza |>
  mutate(
    hogar_con_ocupados = case_when(
      n_ocupados > 0 ~ 1,
      n_ocupados == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    hogar_ocupado_pobre = case_when(
      entra_medicion_pobreza == 1 & pobre == 1 & hogar_con_ocupados == 1 ~ 1,
      entra_medicion_pobreza == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    jefe_ocupado_pobre = case_when(
      entra_medicion_pobreza == 1 & pobre == 1 & jefe_ocupado == 1 ~ 1,
      entra_medicion_pobreza == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    adulto_equiv_grupo = case_when(
      adulto_equiv_hogar < 2 ~ "Menos de 2",
      adulto_equiv_hogar >= 2 & adulto_equiv_hogar < 3 ~ "2 a menos de 3",
      adulto_equiv_hogar >= 3 & adulto_equiv_hogar < 4 ~ "3 a menos de 4",
      adulto_equiv_hogar >= 4 ~ "4 o mas",
      TRUE ~ NA_character_
    ),
    adulto_equiv_grupo = factor(
      adulto_equiv_grupo,
      levels = c("Menos de 2", "2 a menos de 3", "3 a menos de 4", "4 o mas")
    )
  )

# ============================================================
# 10. Resumen general de pobreza
# ============================================================

resumen_pobreza <- hogares_pobreza |>
  group_by(anio, trimestre, periodo, crisis, momento, periodo_crisis, bloque) |>
  summarise(
    hogares_totales = n(),
    hogares_entran_medicion = sum(entra_medicion_pobreza == 1, na.rm = TRUE),
    hogares_fuera_medicion = sum(entra_medicion_pobreza == 0, na.rm = TRUE),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres = sum(pobre == 0, na.rm = TRUE),
    tasa_pobreza_no_ponderada = mean(pobre, na.rm = TRUE),
    tasa_pobreza_ponderada = media_ponderada(pobre, ponderador_ingreso),
    hogares_no_pobres_cerca_linea = sum(cerca_linea_pobreza == 1, na.rm = TRUE),
    hogares_pobres_con_ocupados = sum(hogar_ocupado_pobre == 1, na.rm = TRUE),
    hogares_pobres_con_jefe_ocupado = sum(jefe_ocupado_pobre == 1, na.rm = TRUE),
    itf_mediana = mediana_na(itf[entra_medicion_pobreza == 1]),
    linea_pobreza_mediana = mediana_na(linea_pobreza_hogar),
    ratio_itf_linea_mediana = mediana_na(ratio_itf_linea),
    brecha_pobreza_rel_media = media_na(brecha_pobreza_rel),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

print(resumen_pobreza, n = Inf)

# ============================================================
# 11. Resumen por zona
# ============================================================

resumen_pobreza_gba <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(anio, trimestre, periodo, crisis, momento, periodo_crisis, bloque, gba) |>
  summarise(
    hogares = n(),
    hogares_expandidos = sum(ponderador_ingreso, na.rm = TRUE),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres = sum(pobre == 0, na.rm = TRUE),
    tasa_pobreza_no_ponderada = mean(pobre, na.rm = TRUE),
    tasa_pobreza_ponderada = media_ponderada(pobre, ponderador_ingreso),
    ratio_itf_linea_mediana = mediana_na(ratio_itf_linea),
    brecha_pobreza_abs_mediana_pobres = mediana_na(brecha_pobreza_abs),
    brecha_pobreza_rel_media_pobres = media_na(brecha_pobreza_rel),
    adulto_equiv_media = media_na(adulto_equiv_hogar),
    n_integrantes_media = media_na(n_integrantes),
    n_menores_18_media = media_na(n_menores_18),
    n_ocupados_media = media_na(n_ocupados),
    n_perceptores_media = media_na(n_perceptores_ingreso),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre, gba)

print(resumen_pobreza_gba, n = Inf)

# ============================================================
# 12. Resumen por crisis, momento y zona
# ============================================================

resumen_pobreza_crisis_gba <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(crisis, momento, periodo_crisis, bloque, gba) |>
  summarise(
    hogares = n(),
    hogares_expandidos = sum(ponderador_ingreso, na.rm = TRUE),
    tasa_pobreza_no_ponderada = mean(pobre, na.rm = TRUE),
    tasa_pobreza_ponderada = media_ponderada(pobre, ponderador_ingreso),
    ratio_itf_linea_mediana = mediana_na(ratio_itf_linea),
    brecha_pobreza_rel_media_pobres = media_na(brecha_pobreza_rel),
    adulto_equiv_media = media_na(adulto_equiv_hogar),
    n_integrantes_media = media_na(n_integrantes),
    n_menores_18_media = media_na(n_menores_18),
    prop_menores_media = media_na(prop_menores),
    n_ocupados_media = media_na(n_ocupados),
    n_perceptores_media = media_na(n_perceptores_ingreso),
    adulto_equiv_por_perceptor_media = media_na(adulto_equiv_por_perceptor),
    .groups = "drop"
  ) |>
  arrange(crisis, momento, gba)

print(resumen_pobreza_crisis_gba)

# ============================================================
# 13. Composicion segun situacion de pobreza
# ============================================================

# MODIFICACION:
# este resumen permite comparar hogares pobres y no pobres segun su composicion.
# Es clave para evaluar si la vulnerabilidad se asocia con menores, adultos equivalentes y perceptores.

resumen_composicion_pobreza_gba <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(crisis, momento, periodo_crisis, bloque, gba, situacion_pobreza) |>
  summarise(
    hogares = n(),
    hogares_expandidos = sum(ponderador_ingreso, na.rm = TRUE),
    n_integrantes_media = media_na(n_integrantes),
    adulto_equiv_media = media_na(adulto_equiv_hogar),
    n_menores_18_media = media_na(n_menores_18),
    prop_menores_media = media_na(prop_menores),
    n_dependientes_media = media_na(n_dependientes),
    tasa_dependencia_media = media_na(tasa_dependencia_hogar),
    n_ocupados_media = media_na(n_ocupados),
    n_perceptores_media = media_na(n_perceptores_ingreso),
    adulto_equiv_por_perceptor_media = media_na(adulto_equiv_por_perceptor),
    adulto_equiv_por_ocupado_media = media_na(adulto_equiv_por_ocupado),
    ingreso_adulto_equiv_mediana = mediana_na(ingreso_adulto_equiv),
    ratio_itf_linea_mediana = mediana_na(ratio_itf_linea),
    .groups = "drop"
  ) |>
  arrange(crisis, momento, gba, situacion_pobreza)

print(resumen_composicion_pobreza_gba)

# ============================================================
# 14. Resumen por carga infantil
# ============================================================

resumen_pobreza_carga_infantil <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  filter(!is.na(carga_infantil)) |>
  group_by(crisis, momento, periodo_crisis, bloque, gba, carga_infantil) |>
  summarise(
    hogares = n(),
    hogares_expandidos = sum(ponderador_ingreso, na.rm = TRUE),
    tasa_pobreza_no_ponderada = mean(pobre, na.rm = TRUE),
    tasa_pobreza_ponderada = media_ponderada(pobre, ponderador_ingreso),
    adulto_equiv_media = media_na(adulto_equiv_hogar),
    n_perceptores_media = media_na(n_perceptores_ingreso),
    ratio_itf_linea_mediana = mediana_na(ratio_itf_linea),
    .groups = "drop"
  ) |>
  arrange(crisis, momento, gba, carga_infantil)

# ============================================================
# 15. Resumen por jefe ocupado
# ============================================================

resumen_pobreza_jefe_ocupado <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(crisis, momento, periodo_crisis, bloque, gba, jefe_ocupado) |>
  summarise(
    hogares = n(),
    hogares_expandidos = sum(ponderador_ingreso, na.rm = TRUE),
    tasa_pobreza_no_ponderada = mean(pobre, na.rm = TRUE),
    tasa_pobreza_ponderada = media_ponderada(pobre, ponderador_ingreso),
    adulto_equiv_media = media_na(adulto_equiv_hogar),
    n_integrantes_media = media_na(n_integrantes),
    n_menores_18_media = media_na(n_menores_18),
    n_perceptores_media = media_na(n_perceptores_ingreso),
    ratio_itf_linea_mediana = mediana_na(ratio_itf_linea),
    brecha_pobreza_rel_media_pobres = media_na(brecha_pobreza_rel),
    .groups = "drop"
  ) |>
  arrange(crisis, momento, gba, jefe_ocupado)

# ============================================================
# 16. Resumen por perceptores de ingreso
# ============================================================

resumen_pobreza_sosten_ingresos <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  filter(!is.na(tipo_sosten_ingresos)) |>
  group_by(crisis, momento, periodo_crisis, bloque, gba, tipo_sosten_ingresos) |>
  summarise(
    hogares = n(),
    hogares_expandidos = sum(ponderador_ingreso, na.rm = TRUE),
    tasa_pobreza_no_ponderada = mean(pobre, na.rm = TRUE),
    tasa_pobreza_ponderada = media_ponderada(pobre, ponderador_ingreso),
    adulto_equiv_media = media_na(adulto_equiv_hogar),
    n_integrantes_media = media_na(n_integrantes),
    n_menores_18_media = media_na(n_menores_18),
    adulto_equiv_por_perceptor_media = media_na(adulto_equiv_por_perceptor),
    dependientes_por_perceptor_media = media_na(dependientes_por_perceptor),
    ingreso_por_perceptor_mediana = mediana_na(ingreso_por_perceptor),
    ratio_itf_linea_mediana = mediana_na(ratio_itf_linea),
    .groups = "drop"
  ) |>
  arrange(crisis, momento, gba, tipo_sosten_ingresos)

# ============================================================
# 17. Resumen por adulto equivalente
# ============================================================

resumen_pobreza_adulto_equiv <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  filter(!is.na(adulto_equiv_grupo)) |>
  group_by(crisis, momento, periodo_crisis, bloque, gba, adulto_equiv_grupo) |>
  summarise(
    hogares = n(),
    hogares_expandidos = sum(ponderador_ingreso, na.rm = TRUE),
    tasa_pobreza_no_ponderada = mean(pobre, na.rm = TRUE),
    tasa_pobreza_ponderada = media_ponderada(pobre, ponderador_ingreso),
    n_integrantes_media = media_na(n_integrantes),
    n_menores_18_media = media_na(n_menores_18),
    n_ocupados_media = media_na(n_ocupados),
    n_perceptores_media = media_na(n_perceptores_ingreso),
    ratio_itf_linea_mediana = mediana_na(ratio_itf_linea),
    brecha_pobreza_rel_media_pobres = media_na(brecha_pobreza_rel),
    .groups = "drop"
  ) |>
  arrange(crisis, momento, gba, adulto_equiv_grupo)

# ============================================================
# 18. Guardado de resultados
# ============================================================

write_rds(
  hogares_pobreza,
  here("data_processed", "hogares_pobreza.rds")
)

write_csv(
  chequeo_union_pobreza,
  here("output", "tablas", "chequeo_union_pobreza.csv")
)

write_csv(
  resumen_pobreza,
  here("output", "tablas", "resumen_pobreza.csv")
)

write_csv(
  resumen_pobreza_gba,
  here("output", "tablas", "resumen_pobreza_gba.csv")
)

write_csv(
  resumen_pobreza_crisis_gba,
  here("output", "tablas", "resumen_pobreza_crisis_gba.csv")
)

write_csv(
  resumen_composicion_pobreza_gba,
  here("output", "tablas", "resumen_composicion_pobreza_gba.csv")
)

write_csv(
  resumen_pobreza_carga_infantil,
  here("output", "tablas", "resumen_pobreza_carga_infantil.csv")
)

write_csv(
  resumen_pobreza_jefe_ocupado,
  here("output", "tablas", "resumen_pobreza_jefe_ocupado.csv")
)

write_csv(
  resumen_pobreza_sosten_ingresos,
  here("output", "tablas", "resumen_pobreza_sosten_ingresos.csv")
)

write_csv(
  resumen_pobreza_adulto_equiv,
  here("output", "tablas", "resumen_pobreza_adulto_equiv.csv")
)

message("Clasificacion de pobreza finalizada.")