# ============================================================
# Clasificacion de pobreza por ingresos
# Proyecto final MVD 2026
# ============================================================

# Objetivo:
# unir hogares EPH, adulto equivalente y CBT de INDEC para clasificar
# hogares pobres en el Gran Buenos Aires.

library(tidyverse)
library(readr)
library(here)

message("Clasificando pobreza por ingresos...")

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
# 2. Union de bases
# ============================================================

hogares_con_cbt <- hogares_eph_gba |>
  left_join(
    adulto_equiv_hogar |>
      select(
        anio,
        trimestre,
        periodo,
        codusu,
        nro_hogar,
        miembros_hogar,
        adulto_equiv_hogar,
        personas_sin_adulto_equiv,
        adulto_equiv_valido,
        sexo_jefe,
        edad_jefe,
        estado_jefe,
        condicion_actividad_jefe,
        cat_ocup_jefe
      ),
    by = c("anio", "trimestre", "periodo", "codusu", "nro_hogar")
  ) |>
  left_join(
    cbt_trimestral_proyecto |>
      select(
        anio,
        trimestre,
        cbt_adulto_equiv,
        cba_adulto_equiv,
        fuente_cbt = fuente
      ),
    by = c("anio", "trimestre")
  )

# ============================================================
# 3. Chequeo de union
# ============================================================

chequeo_union_pobreza <- hogares_con_cbt |>
  group_by(anio, trimestre, periodo) |>
  summarise(
    hogares = n(),
    hogares_sin_adulto_equiv = sum(is.na(adulto_equiv_hogar)),
    hogares_adulto_equiv_invalido = sum(adulto_equiv_valido == 0, na.rm = TRUE),
    hogares_sin_cbt = sum(is.na(cbt_adulto_equiv)),
    hogares_sin_respuesta_ingresos = sum(no_respuesta_ingresos == 1, na.rm = TRUE),
    hogares_ingreso_cero = sum(ingreso_cero == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

print(chequeo_union_pobreza, n = Inf)

# ============================================================
# 4. Clasificacion de pobreza
# ============================================================

hogares_pobreza <- hogares_con_cbt |>
  mutate(
    entra_medicion_pobreza = case_when(
      no_respuesta_ingresos == 0 &
        adulto_equiv_valido == 1 &
        !is.na(itf) &
        !is.na(cbt_adulto_equiv) ~ 1,
      TRUE ~ 0
    ),
    linea_pobreza_hogar = case_when(
      entra_medicion_pobreza == 1 ~ cbt_adulto_equiv * adulto_equiv_hogar,
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
    ratio_itf_cbt = case_when(
      entra_medicion_pobreza == 1 ~ itf / linea_pobreza_hogar,
      TRUE ~ NA_real_
    ),
    cerca_linea_pobreza = case_when(
      entra_medicion_pobreza == 1 & pobre == 0 & ratio_itf_cbt < 1.25 ~ 1,
      entra_medicion_pobreza == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    jefe_ocupado = case_when(
      estado_jefe == 1 ~ 1,
      TRUE ~ 0
    ),
    hogar_pobre_con_jefe_ocupado = case_when(
      pobre == 1 & jefe_ocupado == 1 ~ 1,
      pobre == 0 | is.na(pobre) ~ 0,
      TRUE ~ NA_real_
    ),
    etapa_crisis = case_when(
      anio %in% c(2007, 2008) ~ "Antes de crisis 2009",
      anio %in% c(2010, 2011) ~ "Despues de crisis 2009",
      anio %in% c(2018, 2019) ~ "Antes de crisis 2020",
      anio %in% c(2021, 2022) ~ "Despues de crisis 2020",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 5. Resumen general de pobreza
# ============================================================

resumen_pobreza <- hogares_pobreza |>
  group_by(anio, trimestre, periodo) |>
  summarise(
    hogares_totales = n(),
    hogares_entran_medicion = sum(entra_medicion_pobreza == 1, na.rm = TRUE),
    hogares_fuera_medicion = sum(entra_medicion_pobreza == 0, na.rm = TRUE),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres = sum(pobre == 0, na.rm = TRUE),
    tasa_pobreza_no_ponderada = hogares_pobres / hogares_entran_medicion,
    tasa_pobreza_ponderada = sum(pobre * ponderador_ingreso, na.rm = TRUE) /
      sum(ponderador_ingreso[entra_medicion_pobreza == 1], na.rm = TRUE),
    hogares_no_pobres_cerca_linea = sum(cerca_linea_pobreza == 1, na.rm = TRUE),
    hogares_pobres_con_jefe_ocupado = sum(hogar_pobre_con_jefe_ocupado == 1, na.rm = TRUE),
    itf_mediana = median(itf[entra_medicion_pobreza == 1], na.rm = TRUE),
    linea_pobreza_mediana = median(linea_pobreza_hogar, na.rm = TRUE),
    ratio_itf_cbt_mediana = median(ratio_itf_cbt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

print(resumen_pobreza, n = Inf)

# ============================================================
# 6. Resumen por zona
# ============================================================

resumen_pobreza_gba <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(anio, trimestre, periodo, gba) |>
  summarise(
    hogares = n(),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres = sum(pobre == 0, na.rm = TRUE),
    tasa_pobreza_no_ponderada = hogares_pobres / hogares,
    tasa_pobreza_ponderada = sum(pobre * ponderador_ingreso, na.rm = TRUE) /
      sum(ponderador_ingreso, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre, gba)

print(resumen_pobreza_gba, n = Inf)

# ============================================================
# 7. Resumen por etapa de crisis
# ============================================================

resumen_pobreza_etapa <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(etapa_crisis) |>
  summarise(
    hogares = n(),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres = sum(pobre == 0, na.rm = TRUE),
    tasa_pobreza_no_ponderada = hogares_pobres / hogares,
    tasa_pobreza_ponderada = sum(pobre * ponderador_ingreso, na.rm = TRUE) /
      sum(ponderador_ingreso, na.rm = TRUE),
    ratio_itf_cbt_mediana = median(ratio_itf_cbt, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen_pobreza_etapa)

# ============================================================
# 8. Guardado de resultados
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
  resumen_pobreza_etapa,
  here("output", "tablas", "resumen_pobreza_etapa.csv")
)

message("Clasificacion de pobreza finalizada.")