# ============================================================
# Tablas y graficos exploratorios
# Proyecto grupal MVD 2026
# ============================================================

# Objetivo:
# generar tablas y graficos simples para explorar la evolucion
# de la pobreza en hogares del Gran Buenos Aires.

library(tidyverse)
library(readr)
library(here)

message("Generando tablas y graficos exploratorios...")

# ============================================================
# 1. Lectura de base de pobreza
# ============================================================

hogares_pobreza <- read_rds(
  here("data_processed", "hogares_pobreza.rds")
)

# ============================================================
# 2. Tablas exploratorias
# ============================================================

tabla_muestra <- hogares_pobreza |>
  group_by(anio, trimestre, periodo) |>
  summarise(
    hogares_totales = n(),
    hogares_entran_medicion = sum(entra_medicion_pobreza == 1, na.rm = TRUE),
    hogares_fuera_medicion = sum(entra_medicion_pobreza == 0, na.rm = TRUE),
    hogares_sin_respuesta_ingresos = sum(no_respuesta_ingresos == 1, na.rm = TRUE),
    hogares_adulto_equiv_invalido = sum(adulto_equiv_valido == 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

print(tabla_muestra, n = Inf)

tabla_pobreza_periodo <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(anio, trimestre, periodo) |>
  summarise(
    hogares = n(),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres = sum(pobre == 0, na.rm = TRUE),
    tasa_pobreza_no_ponderada = hogares_pobres / hogares,
    tasa_pobreza_ponderada = sum(pobre * ponderador_ingreso, na.rm = TRUE) /
      sum(ponderador_ingreso, na.rm = TRUE),
    itf_mediana = median(itf, na.rm = TRUE),
    linea_pobreza_mediana = median(linea_pobreza_hogar, na.rm = TRUE),
    ratio_itf_cbt_mediana = median(ratio_itf_cbt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

print(tabla_pobreza_periodo, n = Inf)

tabla_pobreza_anio <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(anio) |>
  summarise(
    hogares = n(),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres = sum(pobre == 0, na.rm = TRUE),
    tasa_pobreza_no_ponderada = hogares_pobres / hogares,
    tasa_pobreza_ponderada = sum(pobre * ponderador_ingreso, na.rm = TRUE) /
      sum(ponderador_ingreso, na.rm = TRUE),
    ratio_itf_cbt_mediana = median(ratio_itf_cbt, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio)

print(tabla_pobreza_anio, n = Inf)

tabla_pobreza_gba <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(anio, gba) |>
  summarise(
    hogares = n(),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres = sum(pobre == 0, na.rm = TRUE),
    tasa_pobreza_no_ponderada = hogares_pobres / hogares,
    tasa_pobreza_ponderada = sum(pobre * ponderador_ingreso, na.rm = TRUE) /
      sum(ponderador_ingreso, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, gba)

print(tabla_pobreza_gba, n = Inf)

tabla_pobreza_etapa <- hogares_pobreza |>
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

print(tabla_pobreza_etapa)

tabla_jefe_ocupado <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  mutate(
    jefe_ocupado_texto = case_when(
      jefe_ocupado == 1 ~ "Jefe ocupado",
      jefe_ocupado == 0 ~ "Jefe no ocupado",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(anio, jefe_ocupado_texto) |>
  summarise(
    hogares = n(),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    tasa_pobreza_no_ponderada = hogares_pobres / hogares,
    .groups = "drop"
  ) |>
  arrange(anio, jefe_ocupado_texto)

print(tabla_jefe_ocupado, n = Inf)

tabla_cerca_linea <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  group_by(anio) |>
  summarise(
    hogares = n(),
    hogares_pobres = sum(pobre == 1, na.rm = TRUE),
    hogares_no_pobres_cerca_linea = sum(cerca_linea_pobreza == 1, na.rm = TRUE),
    proporcion_no_pobres_cerca_linea = hogares_no_pobres_cerca_linea / hogares,
    .groups = "drop"
  ) |>
  arrange(anio)

print(tabla_cerca_linea, n = Inf)

# ============================================================
# 3. Guardado de tablas
# ============================================================

write_csv(
  tabla_muestra,
  here("output", "tablas", "eda_tabla_muestra.csv")
)

write_csv(
  tabla_pobreza_periodo,
  here("output", "tablas", "eda_tabla_pobreza_periodo.csv")
)

write_csv(
  tabla_pobreza_anio,
  here("output", "tablas", "eda_tabla_pobreza_anio.csv")
)

write_csv(
  tabla_pobreza_gba,
  here("output", "tablas", "eda_tabla_pobreza_gba.csv")
)

write_csv(
  tabla_pobreza_etapa,
  here("output", "tablas", "eda_tabla_pobreza_etapa.csv")
)

write_csv(
  tabla_jefe_ocupado,
  here("output", "tablas", "eda_tabla_jefe_ocupado.csv")
)

write_csv(
  tabla_cerca_linea,
  here("output", "tablas", "eda_tabla_cerca_linea.csv")
)

# ============================================================
# 4. Graficos simples
# ============================================================

grafico_pobreza_anio <- tabla_pobreza_anio |>
  ggplot(aes(x = factor(anio), y = tasa_pobreza_ponderada)) +
  geom_col() +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100, 0), "%")
  ) +
  labs(
    title = "Tasa de pobreza ponderada en hogares del GBA",
    subtitle = "Años seleccionados",
    x = "Año",
    y = "Tasa de pobreza ponderada"
  ) +
  theme_minimal()

print(grafico_pobreza_anio)

ggsave(
  filename = here("output", "figuras", "grafico_pobreza_anio.png"),
  plot = grafico_pobreza_anio,
  width = 8,
  height = 5
)

grafico_pobreza_periodo <- tabla_pobreza_periodo |>
  ggplot(aes(x = periodo, y = tasa_pobreza_ponderada)) +
  geom_col() +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100, 0), "%")
  ) +
  labs(
    title = "Tasa de pobreza ponderada por trimestre",
    subtitle = "Hogares del GBA",
    x = "Periodo",
    y = "Tasa de pobreza ponderada"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

print(grafico_pobreza_periodo)

ggsave(
  filename = here("output", "figuras", "grafico_pobreza_periodo.png"),
  plot = grafico_pobreza_periodo,
  width = 11,
  height = 5
)

grafico_pobreza_gba <- tabla_pobreza_gba |>
  ggplot(aes(x = factor(anio), y = tasa_pobreza_ponderada, fill = gba)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100, 0), "%")
  ) +
  labs(
    title = "Tasa de pobreza ponderada por zona",
    subtitle = "CABA y Partidos del GBA",
    x = "Año",
    y = "Tasa de pobreza ponderada",
    fill = "Zona"
  ) +
  theme_minimal()

print(grafico_pobreza_gba)

ggsave(
  filename = here("output", "figuras", "grafico_pobreza_gba.png"),
  plot = grafico_pobreza_gba,
  width = 9,
  height = 5
)

grafico_pobreza_etapa <- tabla_pobreza_etapa |>
  ggplot(aes(x = etapa_crisis, y = tasa_pobreza_ponderada)) +
  geom_col() +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100, 0), "%")
  ) +
  labs(
    title = "Tasa de pobreza ponderada por etapa",
    subtitle = "Comparacion antes y despues de las crisis seleccionadas",
    x = "Etapa",
    y = "Tasa de pobreza ponderada"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

print(grafico_pobreza_etapa)

ggsave(
  filename = here("output", "figuras", "grafico_pobreza_etapa.png"),
  plot = grafico_pobreza_etapa,
  width = 9,
  height = 5
)

grafico_muestra <- tabla_muestra |>
  group_by(anio) |>
  summarise(
    hogares_entran_medicion = sum(hogares_entran_medicion, na.rm = TRUE),
    hogares_fuera_medicion = sum(hogares_fuera_medicion, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(hogares_entran_medicion, hogares_fuera_medicion),
    names_to = "tipo",
    values_to = "hogares"
  ) |>
  mutate(
    tipo = case_when(
      tipo == "hogares_entran_medicion" ~ "Entran en medicion",
      tipo == "hogares_fuera_medicion" ~ "Fuera de medicion",
      TRUE ~ tipo
    )
  ) |>
  ggplot(aes(x = factor(anio), y = hogares, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Hogares incluidos y excluidos de la medicion",
    subtitle = "Años seleccionados",
    x = "Año",
    y = "Cantidad de hogares",
    fill = "Estado"
  ) +
  theme_minimal()

print(grafico_muestra)

ggsave(
  filename = here("output", "figuras", "grafico_muestra_medicion.png"),
  plot = grafico_muestra,
  width = 9,
  height = 5
)

grafico_ratio_itf_cbt <- hogares_pobreza |>
  filter(entra_medicion_pobreza == 1) |>
  filter(ratio_itf_cbt <= 4) |>
  ggplot(aes(x = ratio_itf_cbt)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ anio) +
  labs(
    title = "Distribucion del ingreso familiar respecto de la linea de pobreza",
    subtitle = "Valores menores a 1 indican hogares pobres",
    x = "Ingreso total familiar dividido por linea de pobreza",
    y = "Cantidad de hogares"
  ) +
  theme_minimal()

print(grafico_ratio_itf_cbt)

ggsave(
  filename = here("output", "figuras", "grafico_ratio_itf_cbt.png"),
  plot = grafico_ratio_itf_cbt,
  width = 10,
  height = 6
)

message("Tablas y graficos exploratorios finalizados.")