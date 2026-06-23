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
    presencia_menores = factor(
      if_else(n_menores_18 > 0, "Con menores", "Sin menores"),
      levels = c("Sin menores", "Con menores")
    ),
    estado_jefe = case_when(
      jefe_ocupado == 1 ~ "Jefe ocupado",
      jefe_ocupado == 0 ~ "Jefe no ocupado",
      TRUE ~ NA_character_
    ),
    estado_jefe = factor(
      estado_jefe,
      levels = c("Jefe ocupado", "Jefe no ocupado")
    )
  )

# ============================================================
# 1. Serie temporal de incidencia de pobreza por zona
# ============================================================

tabla_g1 <- base_medicion |>
  group_by(crisis, momento, fecha, anio, trimestre, gba) |>
  summarise(
    tasa_pobreza = media_ponderada(pobre, ponderador_ingreso),
    hogares_muestrales = n(),
    .groups = "drop"
  )

franjas_crisis <- tribble(
  ~crisis,       ~inicio,                ~fin,
  "Crisis 2009", as.Date("2009-01-01"), as.Date("2009-12-31"),
  "Crisis 2020", as.Date("2020-01-01"), as.Date("2020-12-31")
) |>
  mutate(crisis = factor(crisis, levels = levels(base_medicion$crisis)))

# Une con línea punteada el último dato anterior y el primero posterior.
# La serie sólida se corta porque esos trimestres no fueron observados.
puentes_g1 <- tabla_g1 |>
  group_by(crisis, gba, momento) |>
  slice(if (first(momento) == "Antes") n() else 1) |>
  ungroup() |>
  select(crisis, gba, momento, fecha, tasa_pobreza) |>
  pivot_wider(
    names_from = momento,
    values_from = c(fecha, tasa_pobreza),
    names_sep = "_"
  )

g1 <- ggplot(
  tabla_g1,
  aes(fecha, tasa_pobreza, color = gba, group = interaction(gba, momento))
) +
  geom_rect(
    data = franjas_crisis,
    aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey65",
    alpha = 0.22
  ) +
  geom_line(linewidth = 1) +
  geom_segment(
    data = puentes_g1,
    aes(
      x = fecha_Antes,
      xend = fecha_Después,
      y = tasa_pobreza_Antes,
      yend = tasa_pobreza_Después,
      color = gba
    ),
    inherit.aes = FALSE,
    linewidth = 0.8,
    linetype = "dotted"
  ) +
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
    subtitle = "La franja gris marca el año de crisis y la línea punteada, el período sin observaciones",
    x = NULL,
    y = "Tasa de pobreza",
    color = NULL,
    caption = paste(
      "Fuente: elaboración propia con EPH-INDEC.",
      "Las líneas sólidas solo unen trimestres observados."
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
    ),
    panel.spacing.x = unit(1.4, "cm"),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.6)
  )

g1
guardar_grafico(g1, "01_serie_pobreza_zona", 12, 7)
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

etiquetas_cambio_g2 <- tabla_g2 |>
  select(crisis, momento, gba, tasa_pobreza) |>
  pivot_wider(names_from = momento, values_from = tasa_pobreza) |>
  mutate(
    cambio_pp = 100 * (`Después` - Antes),
    x = 1.5,
    y = (Antes + `Después`) / 2
  )

g2 <- ggplot(tabla_g2, aes(momento, tasa_pobreza, color = gba, group = gba)) +
  geom_line(
    linewidth = 1.2,
    arrow = arrow(length = unit(0.16, "cm"), type = "closed")
  ) +
  geom_point(size = 3) +
  geom_text(
    aes(label = label_percent(accuracy = 0.1)(tasa_pobreza)),
    vjust = -0.8, show.legend = FALSE, size = 3.5
  ) +
  geom_label(
    data = etiquetas_cambio_g2,
    aes(x = x, y = y, label = number(cambio_pp, accuracy = 0.1, suffix = " pp"), color = gba),
    inherit.aes = FALSE,
    fill = "white",
    linewidth = 0,
    size = 3.2,
    show.legend = FALSE
  ) +
  facet_wrap(~ crisis, nrow = 1) +
  scale_color_manual(values = colores_zona) +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, NA), expand = expansion(mult = c(0.03, 0.15))) +
  labs(
    title = "Cambio de la pobreza alrededor de cada crisis",
    subtitle = "Las flechas y etiquetas muestran el cambio antes/después en puntos porcentuales",
    x = NULL, y = "Tasa de pobreza", color = NULL,
    caption = paste(
      "Fuente: elaboración propia con EPH-INDEC.",
      "Antes/después: 2007-2008/2010-2011 y 2018-2019/2021-2022; no se observan 2009 ni 2020."
    )
  ) +
  tema_proyecto +
  theme(
    panel.spacing.x = unit(1.5, "cm"),
    panel.border = element_rect(color = "grey65", fill = NA, linewidth = 0.7),
    strip.background = element_rect(fill = "grey92", color = "grey65")
  )

guardar_grafico(g2, "02_cambio_antes_despues", 11, 6.5)
guardar_tabla(tabla_g2, "02_cambio_antes_despues")
g2
# ============================================================
# 3. Brecha territorial de pobreza
# ============================================================

tabla_g3 <- tabla_g2 |>
  mutate(gba = as.character(gba)) |>
  pivot_wider(names_from = gba, values_from = tasa_pobreza) |>
  mutate(
    brecha_pp = 100 * (`Partidos del GBA` - CABA)
  )

etiquetas_cambio_g3 <- tabla_g3 |>
  select(crisis, momento, brecha_pp) |>
  pivot_wider(names_from = momento, values_from = brecha_pp) |>
  mutate(cambio_brecha_pp = `Después` - Antes)

g3 <- ggplot(tabla_g3, aes(momento, brecha_pp, group = crisis)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_line(
    color = colores_zona[["Partidos del GBA"]],
    linewidth = 1.2,
    arrow = arrow(length = unit(0.16, "cm"), type = "closed")
  ) +
  geom_point(color = colores_zona[["Partidos del GBA"]], size = 3.5) +
  geom_text(
    aes(label = number(brecha_pp, accuracy = 0.1, suffix = " pp")),
    vjust = -0.9,
    size = 3.5
  ) +
  geom_label(
    data = etiquetas_cambio_g3,
    aes(
      x = 1.5,
      y = pmax(Antes, `Después`),
      label = paste0(
        "Cambio de la brecha: ",
        sprintf("%+.1f pp", cambio_brecha_pp)
      )
    ),
    inherit.aes = FALSE,
    vjust = -1.3,
    fill = "white",
    linewidth = 0,
    size = 3.2
  ) +
  facet_wrap(~ crisis, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.3))) +
  labs(
    title = "¿La crisis amplió la brecha de pobreza entre GBA y CABA?",
    subtitle = "Se compara directamente la diferencia territorial antes y después de cada crisis",
    x = NULL,
    y = "Brecha Partidos del GBA - CABA (puntos porcentuales)",
    caption = paste(
      "Fuente: elaboración propia con EPH-INDEC.",
      "La baja de 2009 refiere a la brecha entre los bloques disponibles, no a una medición durante 2009."
    )
  ) +
  tema_proyecto +
  theme(
    legend.position = "none",
    panel.spacing.x = unit(1.5, "cm"),
    panel.border = element_rect(color = "grey65", fill = NA, linewidth = 0.7),
    strip.background = element_rect(fill = "grey92", color = "grey65")
  )
g3
guardar_grafico(g3, "03_brecha_territorial", 11, 6.5)
guardar_tabla(tabla_g3, "03_brecha_territorial")

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

resumen_g5 <- tabla_g5 |>
  group_by(crisis, situacion_pobreza, gba) |>
  summarise(
    mediana_adulto_equiv = mediana_ponderada(adulto_equiv_hogar, ponderador_ingreso),
    .groups = "drop"
  )

g5 <- ggplot(tabla_g5, aes(gba, adulto_equiv_hogar, fill = gba)) +
  geom_violin(
    aes(weight = ponderador_ingreso),
    alpha = 0.42,
    trim = TRUE,
    quantiles = NULL,
    color = NA
  ) +
  geom_point(
    data = resumen_g5,
    aes(x = gba, y = mediana_adulto_equiv),
    inherit.aes = FALSE,
    shape = 23,
    size = 3.2,
    fill = "white",
    color = "black"
  ) +
  geom_text(
    data = resumen_g5,
    aes(x = gba, y = mediana_adulto_equiv, label = number(mediana_adulto_equiv, accuracy = 0.01)),
    inherit.aes = FALSE,
    vjust = -1.2,
    size = 3.1
  ) +
  facet_grid(rows = vars(crisis), cols = vars(situacion_pobreza)) +
  scale_fill_manual(values = colores_zona) +
  coord_cartesian(ylim = c(0, quantile(tabla_g5$adulto_equiv_hogar, 0.99, na.rm = TRUE))) +
  labs(
    title = "Los hogares pobres sostienen más adultos equivalentes",
    subtitle = "Cuatro comparaciones: crisis en las filas y pobreza en las columnas; el rombo indica la mediana ponderada",
    x = NULL, y = "Adultos equivalentes del hogar", fill = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC. Escala recortada en el percentil 99."
  ) +
  tema_proyecto +
  theme(
    axis.text.x = element_text(angle = 12, hjust = 1),
    panel.spacing = unit(0.8, "cm"),
    panel.border = element_rect(color = "grey72", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey92", color = "grey72")
  )
g5
guardar_grafico(g5, "05_adultos_equivalentes", 12, 8)

# ============================================================
# 6. Presencia de menores y tasa de pobreza
# ============================================================

tabla_g6 <- base_medicion |>
  group_by(crisis, momento, gba, presencia_menores) |>
  summarise(
    tasa_pobreza = media_ponderada(pobre, ponderador_ingreso),
    .groups = "drop"
  )

colores_menores <- c("Sin menores" = "#5B8E7D", "Con menores" = "#C44536")
tipos_linea_zona <- c("CABA" = "solid", "Partidos del GBA" = "dashed")
formas_zona <- c("CABA" = 16, "Partidos del GBA" = 17)

g6 <- ggplot(
  tabla_g6,
  aes(
    momento,
    tasa_pobreza,
    color = presencia_menores,
    linetype = gba,
    shape = gba,
    group = interaction(gba, presencia_menores)
  )
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 3.2) +
  facet_wrap(~ crisis, nrow = 1) +
  scale_color_manual(values = colores_menores) +
  scale_linetype_manual(values = tipos_linea_zona) +
  scale_shape_manual(values = formas_zona) +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, NA)) +
  labs(
    title = "¿Cómo cambió la pobreza en hogares con y sin menores?",
    subtitle = "Color: presencia de menores. Tipo de línea y forma: territorio.",
    x = NULL,
    y = "Tasa de pobreza",
    color = "Composición del hogar",
    linetype = "Territorio",
    shape = "Territorio",
    caption = paste(
      "Fuente: elaboración propia con EPH-INDEC.",
      "Antes/después usa los bloques 2007-2008/2010-2011 y 2018-2019/2021-2022."
    )
  ) +
  tema_proyecto +
  theme(
    panel.spacing.x = unit(1.5, "cm"),
    panel.border = element_rect(color = "grey65", fill = NA, linewidth = 0.7),
    strip.background = element_rect(fill = "grey92", color = "grey65")
  )
g6
guardar_grafico(g6, "06_menores_y_pobreza", 12, 7)
guardar_tabla(tabla_g6, "06_menores_y_pobreza")

# ============================================================
# 7. Carga familiar por perceptor de ingreso
# ============================================================

tabla_g7 <- base_medicion |>
  filter(
    momento == "Después",
    is.finite(adulto_equiv_por_perceptor)
  ) |>
  group_by(crisis, gba) |>
  summarise(
    carga_mediana = mediana_ponderada(
      adulto_equiv_por_perceptor,
      ponderador_ingreso
    ),
    .groups = "drop"
  )

tabla_g7_brecha <- tabla_g7 |>
  mutate(gba = as.character(gba)) |>
  pivot_wider(
    names_from = gba,
    values_from = carga_mediana
  ) |>
  mutate(
    brecha_rel = (`Partidos del GBA` / CABA) - 1,
    etiqueta_brecha = paste0(
      "GBA: +",
      number(100 * brecha_rel, accuracy = 1),
      "% vs CABA"
    ),
    y_etiqueta = pmax(CABA, `Partidos del GBA`) + 0.10
  )

pos_g7 <- position_dodge(width = 0.72)

g7 <- ggplot(
  tabla_g7,
  aes(
    x = crisis,
    y = carga_mediana,
    fill = gba
  )
) +
  geom_col(
    position = pos_g7,
    width = 0.62,
    alpha = 0.92
  ) +
  geom_text(
    aes(label = number(carga_mediana, accuracy = 0.01)),
    position = pos_g7,
    vjust = -0.45,
    size = 3.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_label(
    data = tabla_g7_brecha,
    aes(
      x = crisis,
      y = y_etiqueta,
      label = etiqueta_brecha
    ),
    inherit.aes = FALSE,
    fill = "white",
    color = colores_zona[["Partidos del GBA"]],
    fontface = "bold",
    linewidth = 0.25,
    size = 3.5
  ) +
  scale_fill_manual(values = colores_zona) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(0, max(tabla_g7_brecha$y_etiqueta, na.rm = TRUE) + 0.12),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "En Partidos del GBA cada perceptor sostiene más carga económica",
    subtitle = "Después de cada crisis; 1,27 significa que cada perceptor sostiene 1,27 adultos equivalentes del hogar",
    x = NULL,
    y = "Adultos equivalentes por perceptor de ingreso",
    fill = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC. Excluye hogares sin perceptores."
  ) +
  tema_proyecto +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold"),
    legend.position = "top"
  )

g7
guardar_grafico(g7, "07_carga_por_perceptor", 10, 6)
guardar_tabla(tabla_g7_brecha, "07_carga_por_perceptor")

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

etiquetas_cambio_g8 <- tabla_g8 |>
  select(crisis, gba, estado_jefe, tasa_pobreza) |>
  pivot_wider(names_from = estado_jefe, values_from = tasa_pobreza) |>
  mutate(
    aumento_pp = 100 * (`Jefe no ocupado` - `Jefe ocupado`),
    x = 1.5,
    y = (`Jefe ocupado` + `Jefe no ocupado`) / 2
  )

g8 <- ggplot(tabla_g8, aes(estado_jefe, tasa_pobreza, color = gba, group = gba)) +
  geom_line(
    linewidth = 1.05,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  ) +
  geom_point(size = 3) +
  geom_label(
    data = etiquetas_cambio_g8,
    aes(x = x, y = y, label = paste0("+", number(aumento_pp, accuracy = 0.1), " pp"), color = gba),
    inherit.aes = FALSE,
    fill = "white",
    linewidth = 0,
    size = 3.1,
    show.legend = FALSE
  ) +
  facet_wrap(~ crisis, nrow = 1) +
  scale_color_manual(values = colores_zona) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0.03, 0.15))
  ) +
  labs(
    title = "La pobreza aumenta cuando el jefe no está ocupado",
    subtitle = "Ocupado se ubica a la izquierda y no ocupado a la derecha; las etiquetas muestran el aumento",
    x = NULL, y = "Tasa de pobreza", color = NULL,
    caption = "Fuente: elaboración propia con EPH-INDEC."
  ) +
  tema_proyecto +
  theme(
    panel.spacing.x = unit(1.5, "cm"),
    panel.border = element_rect(color = "grey65", fill = NA, linewidth = 0.7),
    strip.background = element_rect(fill = "grey92", color = "grey65")
  )
g8
guardar_grafico(g8, "08_pobreza_jefe_ocupado", 11, 6.5)
guardar_tabla(tabla_g8, "08_pobreza_jefe_ocupado")

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
  ungroup()

# Ordena los mecanismos por el mayor cambio absoluto entre antes y después
# observado en cualquiera de las dos crisis.
orden_g9 <- tabla_g9 |>
  select(crisis, momento, mecanismo, brecha_estandarizada) |>
  pivot_wider(names_from = momento, values_from = brecha_estandarizada) |>
  mutate(cambio_temporal = `Después` - Antes) |>
  group_by(mecanismo) |>
  summarise(
    magnitud_cambio = max(abs(cambio_temporal), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(magnitud_cambio))

tabla_g9 <- tabla_g9 |>
  mutate(
    mecanismo = factor(
      mecanismo,
      levels = rev(orden_g9$mecanismo)
    )
  )

g9 <- ggplot(
  tabla_g9,
  aes(
    x = momento,
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
  facet_grid(
    cols = vars(crisis),
    scales = "free_x",
    space = "free_x"
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
    subtitle = paste0(
      "Filas ordenadas por el mayor cambio antes/después.\n",
      "Valores positivos indican mayor vulnerabilidad relativa en Partidos del GBA."
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
    legend.position = "right",
    panel.spacing.x = unit(1.4, "cm"),
    panel.border = element_rect(color = "grey65", fill = NA, linewidth = 0.7),
    strip.background = element_rect(fill = "grey90", color = "grey65")
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