# ============================================================
# Calculo de adulto equivalente y composicion del hogar
# Proyecto Grupal MVD 2026
# ============================================================

# Se busca asignar a cada persona un coeficiente de "adulto equivalente" segun sexo y edad.
# Es la base para poder construir una aproximacion a cuanto consume cada persona respecto de un adulto
# de referencia. 

# Ex post se suman los coefs individuales a nivel de hogar para asignar a este su cantidad total de 
# "adultos equivalentes". 
# Deriva de aca el uso de esta cantidad para ajustar ingresos familiares por tamaño/composicion del hogar. 

# Modificación:
# ahora tambien se construyen variables de composicion demografica, laboral y de perceptores.
# Esto se agrega porque ahora la pregunta no mira solo cuanta pobreza hay, sino si la estructura de 
# los hogares ayuda a explicar la mayor vulnerabilidad de Partidos del GBA.

library(tidyverse)
library(readr)
library(here)

message("Calculando adulto equivalente y composicion del hogar....")

# ============================================================
# 1. Lectura de bases limpias
# ============================================================

individuos_eph_gba <- read_rds(
  here("data_processed", "individuos_eph_gba.rds")
)

# Aqui uso la base de hogares para verificar consistencia vs n_declarado de miembros del hogar
# y para incorporar ingresos y ponderadores a la base agregada.
hogares_eph_gba <- read_rds(
  here("data_processed", "hogares_eph_gba.rds")
)

# Chequeo minimo de variables necesarias.
# Se deja solo este control porque si falta alguna de estas variables,
# el calculo de adulto equivalente o composicion del hogar queda mal desde el inicio.

variables_indispensables_individuos <- c(
  "id_hogar",
  "anio",
  "ano4",
  "trimestre",
  "periodo",
  "periodo_orden",
  "anio_trimestre",
  "fecha_trimestre",
  "codusu",
  "nro_hogar",
  "aglomerado",
  "gba",
  "componente",
  "ch03",
  "jefe_hogar",
  "ch04",
  "sexo",
  "ch06",
  "estado",
  "condicion_actividad",
  "cat_ocup"
)

variables_indispensables_hogares <- c(
  "id_hogar",
  "ix_tot",
  "itf",
  "ipcf",
  "pondera",
  "pondih",
  "ponderador_ingreso",
  "no_respuesta_ingresos",
  "ingreso_cero"
)

faltan_individuos <- setdiff(variables_indispensables_individuos, names(individuos_eph_gba))
faltan_hogares <- setdiff(variables_indispensables_hogares, names(hogares_eph_gba))

if (length(faltan_individuos) > 0) {
  stop(
    "Faltan variables indispensables en individuos_eph_gba: ",
    paste(faltan_individuos, collapse = ", "),
    call. = FALSE
  )
}

if (length(faltan_hogares) > 0) {
  stop(
    "Faltan variables indispensables en hogares_eph_gba: ",
    paste(faltan_hogares, collapse = ", "),
    call. = FALSE
  )
}

# ============================================================
# 2. Funciones auxiliares
# ============================================================

# Funcion simple para divisiones donde el denominador puede ser 0.
# Evita generar Inf cuando no hay adultos activos, ocupados o perceptores.

dividir_seguro <- function(numerador, denominador) {
  
  case_when(
    !is.na(denominador) & denominador > 0 ~ numerador / denominador,
    TRUE ~ NA_real_
  )
}

# Funcion para convertir ingresos individuales si vienen como texto.

convertir_numero_eph <- function(x) {
  
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

# Identifica la variable de ingreso individual disponible.
# p47t es el ingreso individual total; si no aparece, se usa p21 como aproximacion laboral.

elegir_variable_ingreso_individual <- function(base) {
  
  posibles <- c("p47t", "p47t_new", "p21")
  presentes <- posibles[posibles %in% names(base)]
  
  if (length(presentes) == 0) {
    return(NA_character_)
  }
  
  presentes[1]
}

# ============================================================
# 3. Tabla de adulto equivalente
# ============================================================

# Ajusta manualmente tabla de equivalencias por sexo y tramo etario.
tabla_adulto_equivalente <- tribble(
  ~sexo,    ~edad_desde, ~edad_hasta, ~adulto_equiv,
  "Varon", 0,           0,           0.35,
  "Varon", 1,           1,           0.37,
  "Varon", 2,           2,           0.46,
  "Varon", 3,           3,           0.51,
  "Varon", 4,           4,           0.55,
  "Varon", 5,           5,           0.60,
  "Varon", 6,           6,           0.64,
  "Varon", 7,           7,           0.66,
  "Varon", 8,           8,           0.68,
  "Varon", 9,           9,           0.69,
  "Varon", 10,          10,          0.79,
  "Varon", 11,          11,          0.82,
  "Varon", 12,          12,          0.85,
  "Varon", 13,          13,          0.90,
  "Varon", 14,          14,          0.96,
  "Varon", 15,          15,          1.00,
  "Varon", 16,          16,          1.03,
  "Varon", 17,          17,          1.04,
  "Varon", 18,          29,          1.02,
  "Varon", 30,          45,          1.00,
  "Varon", 46,          60,          1.00,
  "Varon", 61,          75,          0.83,
  "Varon", 76,          120,         0.74,
  "Mujer", 0,           0,           0.35,
  "Mujer", 1,           1,           0.37,
  "Mujer", 2,           2,           0.46,
  "Mujer", 3,           3,           0.51,
  "Mujer", 4,           4,           0.55,
  "Mujer", 5,           5,           0.60,
  "Mujer", 6,           6,           0.64,
  "Mujer", 7,           7,           0.66,
  "Mujer", 8,           8,           0.68,
  "Mujer", 9,           9,           0.69,
  "Mujer", 10,          10,          0.70,
  "Mujer", 11,          11,          0.72,
  "Mujer", 12,          12,          0.74,
  "Mujer", 13,          13,          0.76,
  "Mujer", 14,          14,          0.76,
  "Mujer", 15,          15,          0.77,
  "Mujer", 16,          16,          0.77,
  "Mujer", 17,          17,          0.77,
  "Mujer", 18,          29,          0.76,
  "Mujer", 30,          45,          0.77,
  "Mujer", 46,          60,          0.76,
  "Mujer", 61,          75,          0.67,
  "Mujer", 76,          120,         0.63
)

# Cada fila indica el coeficiente de consumo equivalente correspondiente al grupo especifico dado.

write_csv(
  tabla_adulto_equivalente,
  here("output", "tablas", "tabla_adulto_equivalente.csv")
)

# ============================================================
# 4. Asignacion de adulto equivalente
# ============================================================

# En lugar de unir cada persona con todos los tramos posibles de su sexo,
# expandimos primero la tabla de adulto equivalente para que haya una fila por sexo y edad.
# Asi, cada persona se une directamente con un unico valor posible de adulto equivalente.

tabla_adulto_equivalente_expandida <- tabla_adulto_equivalente |>
  rowwise() |>
  mutate(
    edad = list(seq(edad_desde, edad_hasta))
  ) |>
  unnest(edad) |>
  ungroup() |>
  select(
    sexo,
    edad,
    adulto_equiv
  )

## MODIFICACIÓN:
# se identifica una variable de ingreso individual para poder contar perceptores por hogar.
# Esto es necesario para analizar si la pobreza se relaciona con la cantidad de personas que sostienen economicamente al hogar.

variable_ingreso_individual <- elegir_variable_ingreso_individual(individuos_eph_gba)

if (is.na(variable_ingreso_individual)) {
  warning(
    "No se encontro p47t, p47t_new ni p21. ",
    "Las variables de perceptores quedaran sin dato."
  )
} else {
  message("Variable usada para perceptores de ingreso: ", variable_ingreso_individual)
}

individuos_adulto_equiv <- individuos_eph_gba |>
  mutate(
    id_persona = row_number(),
    
    # Correccion incorporada:
    # la EPH puede traer ch06 = -1 para menores de 1 año.
    # Para adulto equivalente se los toma como edad 0, porque la tabla oficial
    # contempla el tramo de menores de 1 año con coeficiente 0.35.
    # Se conserva edad_original para no perder el dato tal como venia en la base.
    edad_original = ch06,
    edad = case_when(
      ch06 == -1 ~ 0,
      ch06 >= 0 ~ ch06,
      TRUE ~ NA_real_
    ),
    
    ingreso_individual = if (is.na(variable_ingreso_individual)) {
      NA_real_
    } else {
      convertir_numero_eph(.data[[variable_ingreso_individual]])
    },
    ingreso_individual = case_when(
      !is.na(ingreso_individual) & ingreso_individual > 0 ~ ingreso_individual,
      TRUE ~ NA_real_
    ),
    perceptor_ingreso = case_when(
      !is.na(ingreso_individual) & ingreso_individual > 0 ~ 1,
      is.na(variable_ingreso_individual) ~ NA_real_,
      TRUE ~ 0
    )
  ) |>
  left_join(
    tabla_adulto_equivalente_expandida,
    by = c("sexo", "edad")
  ) |>
  mutate(
    adulto_equiv_faltante = case_when(
      is.na(adulto_equiv) ~ 1,
      TRUE ~ 0
    )
  )

# ============================================================
# 5. Adulto equivalente y composicion por hogar
# ============================================================

# Agrega los coeficientes individuales a nivel hogar.
# La suma de adulto_equiv representa la cantidad total de adultos equivalentes del hogar.
# Si al menos una persona del hogar no tiene adulto equivalente valido, se invalida el total del hogar,
# logicamente, sino la suma quedaria incompleta y subestimaria la necesidad economica del hogar.

## MODIFICACIÓN
# aca tambien se agregan variables demograficas y laborales.
# Esto se agrega porque la nueva hipotesis incluye el tamaño del hogar, menores,
# dependientes, ocupados y perceptores, no solo de pobreza total.

adulto_equiv_hogar <- individuos_adulto_equiv |>
  group_by(
    id_hogar,
    anio,
    ano4,
    trimestre,
    periodo,
    periodo_orden,
    anio_trimestre,
    fecha_trimestre,
    codusu,
    nro_hogar,
    aglomerado,
    gba
  ) |>
  summarise(
    miembros_hogar = n(),
    n_integrantes = n(),
    
    personas_sin_adulto_equiv = sum(adulto_equiv_faltante == 1, na.rm = TRUE),
    adulto_equiv_hogar = case_when(
      personas_sin_adulto_equiv == 0 ~ sum(adulto_equiv, na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    adulto_equiv_valido = case_when(
      personas_sin_adulto_equiv == 0 ~ 1,
      TRUE ~ 0
    ),
    
    # Variables de composicion demografica
    n_menores_18 = sum(!is.na(edad) & edad < 18),
    n_adultos_18_64 = sum(!is.na(edad) & edad >= 18 & edad <= 64),
    n_mayores_65 = sum(!is.na(edad) & edad >= 65),
    n_dependientes = n_menores_18 + n_mayores_65,
    
    # Variables de composicion laboral
    n_ocupados = sum(estado == 1, na.rm = TRUE),
    
    # Variables de perceptores
    n_perceptores_ingreso = case_when(
      is.na(variable_ingreso_individual) ~ NA_real_,
      TRUE ~ sum(perceptor_ingreso == 1, na.rm = TRUE)
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    prop_menores = dividir_seguro(n_menores_18, n_integrantes),
    tasa_dependencia_hogar = dividir_seguro(n_dependientes, n_adultos_18_64),
    menores_por_adulto_activo = dividir_seguro(n_menores_18, n_adultos_18_64),
    adulto_equiv_por_adulto_activo = dividir_seguro(adulto_equiv_hogar, n_adultos_18_64),
    
    tipo_estructura_etaria = case_when(
      n_menores_18 == 0 & n_mayores_65 == 0 ~ "Solo adultos 18-64",
      n_menores_18 > 0 & n_mayores_65 == 0 ~ "Con menores",
      n_menores_18 == 0 & n_mayores_65 > 0 ~ "Con mayores de 65",
      n_menores_18 > 0 & n_mayores_65 > 0 ~ "Con menores y mayores",
      TRUE ~ NA_character_
    ),
    tipo_estructura_etaria = factor(
      tipo_estructura_etaria,
      levels = c(
        "Solo adultos 18-64",
        "Con menores",
        "Con mayores de 65",
        "Con menores y mayores"
      )
    ),
    
    carga_infantil = case_when(
      n_menores_18 == 0 ~ "Sin menores",
      n_menores_18 == 1 ~ "1 menor",
      n_menores_18 == 2 ~ "2 menores",
      n_menores_18 >= 3 ~ "3 o mas menores",
      TRUE ~ NA_character_
    ),
    carga_infantil = factor(
      carga_infantil,
      levels = c("Sin menores", "1 menor", "2 menores", "3 o mas menores")
    ),
    
    menores_un_adulto_activo = case_when(
      n_menores_18 > 0 & n_adultos_18_64 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    # Se usa 4 o mas integrantes como umbral simple de hogar numeroso.
    # Si despues se decide otro umbral, se cambia aca.
    hogar_numeroso = case_when(
      n_integrantes >= 4 ~ 1,
      TRUE ~ 0
    ),
    
    tipo_insercion_laboral = case_when(
      n_ocupados == 0 ~ "Sin ocupados",
      n_ocupados == 1 ~ "1 ocupado",
      n_ocupados >= 2 ~ "2 o mas ocupados",
      TRUE ~ NA_character_
    ),
    tipo_insercion_laboral = factor(
      tipo_insercion_laboral,
      levels = c("Sin ocupados", "1 ocupado", "2 o mas ocupados")
    ),
    
    tipo_sosten_ingresos = case_when(
      is.na(n_perceptores_ingreso) ~ "Sin dato",
      n_perceptores_ingreso == 0 ~ "Sin perceptores",
      n_perceptores_ingreso == 1 ~ "1 perceptor",
      n_perceptores_ingreso == 2 ~ "2 perceptores",
      n_perceptores_ingreso >= 3 ~ "3 o mas perceptores",
      TRUE ~ NA_character_
    ),
    tipo_sosten_ingresos = factor(
      tipo_sosten_ingresos,
      levels = c("Sin dato", "Sin perceptores", "1 perceptor", "2 perceptores", "3 o mas perceptores")
    ),
    
    perceptores_por_integrante = dividir_seguro(n_perceptores_ingreso, n_integrantes),
    adulto_equiv_por_perceptor = dividir_seguro(adulto_equiv_hogar, n_perceptores_ingreso),
    adulto_equiv_por_ocupado = dividir_seguro(adulto_equiv_hogar, n_ocupados),
    dependientes_por_perceptor = dividir_seguro(n_dependientes, n_perceptores_ingreso),
    menores_por_perceptor = dividir_seguro(n_menores_18, n_perceptores_ingreso)
  )

# ============================================================
# 6. Informacion del jefe de hogar
# ============================================================

# Se extraen caracteristicas basicas del jefe de hogar.
# Despues nos va a dejar caracterizar hogares por sus atributos sociodemograficos y laborales.

## MODIFICACIÓN:
# se suma jefe_ocupado, porque la nueva pregunta analiza si la pobreza se explica solo por falta
# de empleo o tambien por la relacion entre ingresos, perceptores y carga del hogar.

jefatura_hogar <- individuos_adulto_equiv |>
  group_by(id_hogar) |>
  summarise(
    n_jefes_hogar = sum(jefe_hogar == 1, na.rm = TRUE),
    sexo_jefe = sexo[jefe_hogar == 1][1],
    edad_jefe = edad[jefe_hogar == 1][1],
    estado_jefe = estado[jefe_hogar == 1][1],
    condicion_actividad_jefe = condicion_actividad[jefe_hogar == 1][1],
    cat_ocup_jefe = cat_ocup[jefe_hogar == 1][1],
    jefe_ocupado = case_when(
      estado_jefe == 1 ~ 1,
      !is.na(estado_jefe) ~ 0,
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  )

# Incorpora la informacion de jefe a la base agregada por hogar.
# La union se realiza por el identificador unico de hogar.

adulto_equiv_hogar <- adulto_equiv_hogar |>
  left_join(
    jefatura_hogar,
    by = "id_hogar"
  )

# ============================================================
# 7. Incorporacion de ingresos y ponderadores del hogar
# ============================================================

## MODIFICACIÓN:
# se incorporan ITF, IPCF y ponderadores de la base hogar.
# Todavia no se calcula pobreza; eso queda para el script 04, cuando ya exista la CBT final.
# Aca solo se dejan listas variables de ingresos relativos a perceptores y adultos equivalentes.

hogares_para_join <- hogares_eph_gba |>
  select(
    id_hogar,
    ix_tot,
    itf,
    ipcf,
    pondera_hogar = pondera,
    pondih,
    ponderador_ingreso,
    no_respuesta_ingresos,
    ingreso_cero
  )

adulto_equiv_hogar <- adulto_equiv_hogar |>
  left_join(
    hogares_para_join,
    by = "id_hogar"
  ) |>
  mutate(
    diferencia_miembros = miembros_hogar - ix_tot,
    ingreso_por_perceptor = dividir_seguro(itf, n_perceptores_ingreso),
    ingreso_adulto_equiv = dividir_seguro(itf, adulto_equiv_hogar)
  )

# ============================================================
# 8. Chequeo general
# ============================================================

# MODIFICACIÓN:
# tambien controla hogares sin jefe o con mas de un jefe.
# Se eliminan chequeos descriptivos separados de composicion demografica/laboral.

chequeo_adulto_equivalente <- adulto_equiv_hogar |>
  group_by(anio, trimestre, periodo) |>
  summarise(
    hogares = n(),
    hogares_con_diferencia_miembros = sum(diferencia_miembros != 0, na.rm = TRUE),
    hogares_con_adulto_equiv_valido = sum(adulto_equiv_valido == 1, na.rm = TRUE),
    hogares_con_adulto_equiv_invalido = sum(adulto_equiv_valido == 0, na.rm = TRUE),
    hogares_sin_jefe = sum(is.na(n_jefes_hogar) | n_jefes_hogar == 0, na.rm = TRUE),
    hogares_con_mas_de_un_jefe = sum(n_jefes_hogar > 1, na.rm = TRUE),
    adulto_equiv_media = mean(adulto_equiv_hogar, na.rm = TRUE),
    adulto_equiv_mediana = median(adulto_equiv_hogar, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

print(chequeo_adulto_equivalente, n = Inf)

if (any(chequeo_adulto_equivalente$hogares_con_adulto_equiv_invalido > 0, na.rm = TRUE)) {
  warning("Hay hogares con adulto equivalente invalido. Revisar chequeo_adulto_equivalente.csv")
}

if (any(chequeo_adulto_equivalente$hogares_con_diferencia_miembros > 0, na.rm = TRUE)) {
  warning("Hay diferencias entre miembros reconstruidos desde individuos e ix_tot. Revisar chequeo_adulto_equivalente.csv")
}

if (any(chequeo_adulto_equivalente$hogares_sin_jefe > 0 |
        chequeo_adulto_equivalente$hogares_con_mas_de_un_jefe > 0, na.rm = TRUE)) {
  warning("Hay hogares sin jefe o con mas de un jefe. Revisar chequeo_adulto_equivalente.csv")
}

# ============================================================
# 9. Guardado de resultados
# ============================================================

# La base individual conserva el adulto equivalente asignado a cada persona.
# La base por hogar contiene la suma de adultos equivalentes y variables extra para composicion del hogar.

write_rds(
  individuos_adulto_equiv,
  here("data_processed", "individuos_adulto_equiv.rds")
)

write_rds(
  adulto_equiv_hogar,
  here("data_processed", "adulto_equiv_hogar.rds")
)

write_rds(
  adulto_equiv_hogar,
  here("data_processed", "hogares_composicion.rds")
)

write_csv(
  chequeo_adulto_equivalente,
  here("output", "tablas", "chequeo_adulto_equivalente.csv")
)

message("Calculo de adulto equivalente y composicion del hogar finalizado.")