# ============================================================
# Graficos de diagnostico de vulnerabilidad territorial
# Proyecto final MVD 2026
# ============================================================

library(tidyverse)
library(scales)
library(here)

message("Construyendo graficos de diagnostico...")

dir.create(here("output", "graficos"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "tablas", "graficos"), recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 0. Base, funciones y estilo
# ============================================================

hogares_pobreza <- read_rds(here("data_processed", "hogares_pobreza.rds"))

variables_requeridas <- c(
  "anio", "trimestre", "gba", "crisis", "momento", "entra_medicion_pobreza",
  "pobre", "ponderador_ingreso", "ratio_itf_linea", "brecha_pobreza_rel",
  "adulto_equiv_hogar", "n_menores_18", "n_perceptores_ingreso",
  "perceptores_por_integrante", "adulto_equiv_por_perceptor",
  "ingreso_adulto_equiv", "jefe_ocupado", "situacion_pobreza"
)

faltantes <- setdiff(variables_requeridas, names(hogares_pobreza))
if (length(faltantes) > 0) {
  stop("Faltan variables para graficar: ", paste(faltantes, collapse = ", "))
}

media_ponderada <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

mediana_ponderada <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  orden <- order(x)
  x <- x[orden]
  w <- w[orden]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

guardar_grafico <- function(grafico, nombre, ancho = 10, alto = 6) {
  ggsave(
    here("output", "graficos", paste0(nombre, ".png")),
    grafico, width = ancho, height = alto, dpi = 320, bg = "white"
  )
}

guardar_tabla <- function(tabla, nombre) {
  write_csv(tabla, here("output", "tablas", "graficos", paste0(nombre, ".csv")))
}

colores_zona <- c("CABA" = "#2C7FB8", "Partidos del GBA" = "#D95F0E")
colores_pobreza <- c("No pobre" = "#4C956C", "Pobre" = "#C44536")

tema_proyecto <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey30"),
    plot.caption = element_text(color = "grey40", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

base_medicion <- hogares_pobreza |>
  filter(
    entra_medicion_pobreza == 1,
    gba %in% names(colores_zona),
    crisis %in% c("Crisis 2009", "Crisis 2020"),
    momento %in% c("Antes", "Despues"),
    is.finite(ponderador_ingreso),
    ponderador_ingreso > 0
  ) |>
  mutate(
    gba = factor(gba, levels = names(colores_zona)),
    crisis = factor(crisis, levels = c("Crisis 2009", "Crisis 2020")),
    momento = factor(momento, levels = c("Antes", "Despues"), labels = c("Antes", "Después")),
    fecha = as.Date(sprintf("%d-%02d-01", anio, 1 + 3 * (trimestre - 1))),
    presencia_menores = if_else(n_menores_18 > 0, "Con menores", "Sin menores"),
    estado_jefe = case_when(
      jefe_ocupado == 1 ~ "Jefe ocupado",
      jefe_ocupado == 0 ~ "Jefe no ocupado",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 1. Serie temporal de incidencia de pobreza por zona
# ============================================================

tabla_g1 <- base_medicion |>
  group_by(crisis, fecha, anio, trimestre, gba) |>
  summarise(
    tasa_pobreza = media_ponderada(pobre, ponderador_ingreso),
    hogares_muestrales = n(),
    .groups = "drop"
  )

g1 <- ggplot(
  tabla_g1,
  aes(fecha, tasa_pobreza, color = gba, group = gba)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ crisis, scales = "free_x") +
  scale_color_manual(values = colores_zona) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = breaks_width(0.10),
    labels = label_percent(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "La pobreza se mantiene más alta en Partidos del GBA",
    subtitle = "Incidencia trimestral ponderada de pobreza en hogares",
    x = NULL,
    y = "Tasa de pobreza",
    color = NULL,
    caption = paste(
      "Fuente: elaboración propia con EPH-INDEC.",
      "Los paneles no unen períodos discontinuos."
    )
  ) +
  tema_proyecto +
  theme(
    axis.text.y = element_text(
      size = 11,
      color = "grey20"
    ),
    axis.title.y = element_text(
      size = 12,
      margin = margin(r = 12)
    ),
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.5
    )
  )
guardar_grafico(
  g1,
  "01_serie_pobreza_zona",
  ancho = 11,
  alto = 7
)
g1
guardar_grafico(g1, "01_serie_pobreza_zona", 11, 7)
guardar_tabla(tabla_g1, "01_serie_pobreza_zona")

# ============================================================
# 2. Cambio antes/despues por crisis y zona
# ============================================================

tabla_g2 <- base_medicion |>
  group_by(crisis, momento, gba) |>
  summarise(
    tasa_pobreza = media_ponderada(pobre, ponderador_ingreso),
    .groups = "drop"
  )

g2 <- ggplot(tabla_g2, aes(momento, tasa_pobreza, color = gba, group = gba)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    aes(label = label_percent(accuracy = 0.1)(tasa_pobreza)),
    vjust = -0.8, show.legend = FALSE, size = 3.5
  ) +
  facet_wrap(~ crisis) +
  scale_color_manual(values = colores_zona) +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, NA), expand = expansion(mult = c(0.03, 0.15))) +
  labs(
    title = "Cambio de la pobreza alrededor de cada crisis",
    subtitle = "Promedio ponderado de los años anteriores y posteriores disponibles",
    x = NULL, y = "Tasa de pobreza", color = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC."
  ) +
  tema_proyecto

guardar_grafico(g2, "02_cambio_antes_despues", 10, 6)
guardar_tabla(tabla_g2, "02_cambio_antes_despues")
g2
# ============================================================
# 3. Brecha territorial de pobreza
# ============================================================

tabla_g3 <- tabla_g2 |>
  mutate(gba = as.character(gba)) |>
  pivot_wider(names_from = gba, values_from = tasa_pobreza) |>
  mutate(
    brecha_pp = 100 * (`Partidos del GBA` - CABA),
    periodo = paste(crisis, momento, sep = " · ")
  ) |>
  arrange(brecha_pp) |>
  mutate(periodo = factor(periodo, levels = periodo))

g3 <- ggplot(tabla_g3, aes(brecha_pp, periodo)) +
  geom_vline(xintercept = 0, color = "grey55", linetype = "dashed") +
  geom_segment(aes(x = 0, xend = brecha_pp, yend = periodo), color = "grey70", linewidth = 1) +
  geom_point(color = colores_zona[["Partidos del GBA"]], size = 3.5) +
  geom_text(aes(label = number(brecha_pp, accuracy = 0.1, suffix = " pp")), hjust = -0.15, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(
    title = "Brecha de pobreza entre Partidos del GBA y CABA",
    subtitle = "Valores positivos indican mayor pobreza en Partidos del GBA",
    x = "Diferencia en puntos porcentuales", y = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC."
  ) +
  tema_proyecto + theme(legend.position = "none")
g3
guardar_grafico(g3, "03_brecha_territorial", 10, 5.5)
guardar_tabla(tabla_g3 |> mutate(periodo = as.character(periodo)), "03_brecha_territorial")

# ============================================================
# 4. Distribucion del ingreso respecto de la linea de pobreza
# ============================================================

tabla_g4 <- base_medicion |>
  filter(momento == "Después", between(ratio_itf_linea, 0, 4))

g4 <- ggplot(
  tabla_g4,
  aes(ratio_itf_linea, color = gba, fill = gba, weight = ponderador_ingreso)
) +
  geom_density(alpha = 0.18, linewidth = 0.9, adjust = 1.1) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ crisis, scales = "free_y") +
  scale_color_manual(values = colores_zona) +
  scale_fill_manual(values = colores_zona) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(
    title = "Distribución del ingreso después de las crisis",
    subtitle = "La línea punteada marca el umbral de pobreza (ingreso / línea = 1)",
    x = "Ingreso familiar total / línea de pobreza", y = "Densidad ponderada",
    color = NULL, fill = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC. Se muestran ratios entre 0 y 4."
  ) +
  tema_proyecto
g4
guardar_grafico(g4, "04_distribucion_ratio_ingreso", 11, 6)

# ============================================================
# 5. Adultos equivalentes por zona y pobreza
# ============================================================

tabla_g5 <- base_medicion |>
  filter(momento == "Después", situacion_pobreza %in% c("Pobre", "No pobre")) |>
  mutate(situacion_pobreza = factor(as.character(situacion_pobreza), levels = c("No pobre", "Pobre")))

g5 <- ggplot(tabla_g5, aes(gba, adulto_equiv_hogar, fill = gba, weight = ponderador_ingreso)) +
  geom_violin(alpha = 0.35, trim = TRUE, color = NA) +
  geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.75) +
  facet_grid(situacion_pobreza ~ crisis) +
  scale_fill_manual(values = colores_zona) +
  coord_cartesian(ylim = c(0, quantile(tabla_g5$adulto_equiv_hogar, 0.99, na.rm = TRUE))) +
  labs(
    title = "Los adultos equivalentes elevan la necesidad de ingreso del hogar",
    subtitle = "Distribución posterior a cada crisis, por zona y situación de pobreza",
    x = NULL, y = "Adultos equivalentes del hogar", fill = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC. Escala recortada en el percentil 99."
  ) +
  tema_proyecto + theme(axis.text.x = element_text(angle = 15, hjust = 1))
g5
guardar_grafico(g5, "05_adultos_equivalentes", 11, 7)

# ============================================================
# 6. Presencia de menores y tasa de pobreza
# ============================================================

tabla_g6 <- base_medicion |>
  group_by(crisis, momento, gba, presencia_menores) |>
  summarise(
    tasa_pobreza = media_ponderada(pobre, ponderador_ingreso),
    .groups = "drop"
  )

g6 <- ggplot(tabla_g6, aes(presencia_menores, tasa_pobreza, color = gba, group = gba)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  facet_grid(momento ~ crisis) +
  scale_color_manual(values = colores_zona) +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, NA)) +
  labs(
    title = "Pobreza según presencia de menores en el hogar",
    subtitle = "Comparación territorial antes y después de cada crisis",
    x = NULL, y = "Tasa de pobreza", color = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC."
  ) +
  tema_proyecto
g6
guardar_grafico(g6, "06_menores_y_pobreza", 11, 7)
guardar_tabla(tabla_g6, "06_menores_y_pobreza")

# ============================================================
# 7. Carga familiar por perceptor de ingreso
# ============================================================

tabla_g7 <- base_medicion |>
  filter(momento == "Después", is.finite(adulto_equiv_por_perceptor)) |>
  group_by(crisis, gba) |>
  summarise(
    carga_mediana = mediana_ponderada(adulto_equiv_por_perceptor, ponderador_ingreso),
    .groups = "drop"
  )

g7 <- ggplot(tabla_g7, aes(carga_mediana, crisis, color = gba)) +
  geom_point(size = 4, position = position_dodge(width = 0.45)) +
  geom_text(
    aes(label = number(carga_mediana, accuracy = 0.01)),
    position = position_dodge(width = 0.45), vjust = -1, show.legend = FALSE
  ) +
  scale_color_manual(values = colores_zona) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.03, 0.12))) +
  labs(
    title = "Carga económica por perceptor después de las crisis",
    subtitle = "Mediana ponderada de adultos equivalentes sostenidos por cada perceptor",
    x = "Adultos equivalentes por perceptor", y = NULL, color = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC. Excluye hogares sin perceptores."
  ) +
  tema_proyecto
g7
guardar_grafico(g7, "07_carga_por_perceptor", 10, 5.5)
guardar_tabla(tabla_g7, "07_carga_por_perceptor")

# ============================================================
# 8. Pobreza segun condicion laboral del jefe
# ============================================================

tabla_g8 <- base_medicion |>
  filter(momento == "Después", !is.na(estado_jefe)) |>
  group_by(crisis, gba, estado_jefe) |>
  summarise(
    tasa_pobreza = media_ponderada(pobre, ponderador_ingreso),
    .groups = "drop"
  )

g8 <- ggplot(tabla_g8, aes(estado_jefe, tasa_pobreza, color = gba, group = gba)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~ crisis) +
  scale_color_manual(values = colores_zona) +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, NA)) +
  labs(
    title = "Tener jefe ocupado reduce la pobreza, pero no la elimina",
    subtitle = "Tasa ponderada posterior a cada crisis",
    x = NULL, y = "Tasa de pobreza", color = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC."
  ) +
  tema_proyecto
g8
guardar_grafico(g8, "08_pobreza_jefe_ocupado", 10, 6)
guardar_tabla(tabla_g8, "08_pobreza_jefe_ocupado")

# ============================================================
# 9. Mapa de calor de mecanismos territoriales
# ============================================================
# ============================================================
# 9. Mapa de calor de mecanismos territoriales
# ============================================================

tabla_g9_larga <- base_medicion |>
  group_by(crisis, momento, gba) |>
  summarise(
    `Tasa de pobreza` =
      media_ponderada(pobre, ponderador_ingreso),
    
    `Brecha relativa de pobreza` =
      media_ponderada(brecha_pobreza_rel, ponderador_ingreso),
    
    `Adultos equivalentes` =
      media_ponderada(adulto_equiv_hogar, ponderador_ingreso),
    
    `Presencia de menores` =
      media_ponderada(
        as.numeric(n_menores_18 > 0),
        ponderador_ingreso
      ),
    
    `Perceptores por integrante` =
      media_ponderada(
        perceptores_por_integrante,
        ponderador_ingreso
      ),
    
    `Ingreso por adulto equivalente` =
      media_ponderada(
        ingreso_adulto_equiv,
        ponderador_ingreso
      ),
    
    `Pobreza con jefe ocupado` =
      media_ponderada(
        if_else(jefe_ocupado == 1, pobre, NA_real_),
        ponderador_ingreso
      ),
    
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(crisis, momento, gba),
    names_to = "mecanismo",
    values_to = "valor"
  )

tabla_g9 <- tabla_g9_larga |>
  mutate(gba = as.character(gba)) |>
  pivot_wider(
    names_from = gba,
    values_from = valor
  ) |>
  mutate(
    # Diferencia territorial original
    brecha = `Partidos del GBA` - CABA,
    
    # Los indicadores protectores se invierten.
    # Así, un valor positivo siempre representa
    # mayor vulnerabilidad en Partidos del GBA.
    direccion = if_else(
      mecanismo %in% c(
        "Perceptores por integrante",
        "Ingreso por adulto equivalente"
      ),
      -1,
      1
    ),
    
    brecha_vulnerabilidad = brecha * direccion
  ) |>
  group_by(mecanismo) |>
  mutate(
    brecha_estandarizada = {
      desvio <- sd(
        brecha_vulnerabilidad,
        na.rm = TRUE
      )
      
      if (is.finite(desvio) && desvio > 0) {
        as.numeric(scale(brecha_vulnerabilidad))
      } else {
        rep(0, n())
      }
    }
  ) |>
  ungroup() |>
  mutate(
    columna = paste(
      crisis,
      momento,
      sep = "\n"
    )
  )

g9 <- ggplot(
  tabla_g9,
  aes(
    x = columna,
    y = mecanismo,
    fill = brecha_estandarizada
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  geom_text(
    aes(
      label = number(
        brecha_estandarizada,
        accuracy = 0.1
      )
    ),
    size = 3.2
  ) +
  scale_fill_gradient2(
    low = "#2C7FB8",
    mid = "white",
    high = "#D95F0E",
    midpoint = 0,
    name = "Brecha\nestandarizada"
  ) +
  labs(
    title = "Síntesis de mecanismos de vulnerabilidad territorial",
    subtitle = paste(
      "Valores positivos indican mayor vulnerabilidad relativa",
      "en Partidos del GBA"
    ),
    x = NULL,
    y = NULL,
    caption = paste(
      "Fuente: elaboración propia con EPH-INDEC.",
      "Cada indicador se estandariza para comparar patrones,",
      "no magnitudes originales."
    )
  ) +
  tema_proyecto +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )
g9
guardar_grafico(
  g9,
  "09_heatmap_mecanismos",
  ancho = 11,
  alto = 7
)

guardar_tabla(
  tabla_g9,
  "09_heatmap_mecanismos"
)

message("Graficos finalizados. Revisar output/graficos y output/tablas/graficos.")

