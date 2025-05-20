ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")

##############
# DATA.TABLE #
##############

# Cargamos el paquete data.table
# install.packages("data.table")
library(data.table)

# Convertimos df a data.table
dt <- as.data.table(df)

# Comprobamos clase del objeto
class(dt)

# Selección de filas: vuelos con retraso en llegada
dt[ARRIVAL_DELAY > 60]

# Selección de columnas
dt[, .(DEPARTURE_DELAY, ARRIVAL_DELAY)]

# Crear nueva columna con :=
dt[, DIF_RETRASO := DEPARTURE_DELAY - ARRIVAL_DELAY]

# Conteo de filas por grupo
dt[, .N, by = ORIGIN_AIRPORT]

# Agregación sencilla: media por grupo
dt[, .(media_retraso = mean(ARRIVAL_DELAY, na.rm = TRUE)), by = AIRLINE]

# Ordenación de resultados
dt[order(-ARRIVAL_DELAY)]

# Sintaxis básica general: dt[i, j, by]

# CONCLUSIÓN:
# - data.table es mucho más rápido y eficiente que data.frame
# - Sintaxis concisa y potente
# - Ideal para trabajar con millones de filas sin comprometer rendimiento

##################################
# Sintaxis en detalle data.table #
##################################

library(data.table)
dt <- as.data.table(df)

# FILTRADO DE FILAS
dt[CANCELLED == 1]
dt[ARRIVAL_DELAY > 15 & DEPARTURE_DELAY > 15]
dt[ORIGIN_AIRPORT %in% c("JFK", "ATL", "ORD")]

# SELECCIÓN DE COLUMNAS
dt[, .(FLIGHT_NUMBER, TAIL_NUMBER, DISTANCE)] 

# CREACIÓN Y MODIFICACIÓN DE COLUMNAS
dt[, PUNTUAL := ARRIVAL_DELAY <= 5]
dt[, VELOCIDAD := DISTANCE / (AIR_TIME / 60)]
dt[VELOCIDAD > 800 | VELOCIDAD < 100, VELOCIDAD := NA]

# CLASIFICACIÓN RÁPIDA CON fifelse
dt[, CATEGORIA_DIST := fifelse(DISTANCE < 500, "Corta", "Larga")]

# COMBINACIÓN DE CONDICIONES
dt[, GANANCIA_TIEMPO := DEPARTURE_DELAY - ARRIVAL_DELAY]
dt[, CLASE_RETRASO := fifelse(ARRIVAL_DELAY <= 0, "A tiempo",
                              fifelse(ARRIVAL_DELAY <= 30, "Leve",
                                      fifelse(ARRIVAL_DELAY <= 120, "Moderado", "Grave")))]

# ELIMINACIÓN DE COLUMNAS
dt[, VELOCIDAD := NULL]

# COMPARACIÓN DE RENDIMIENTO
library(microbenchmark)
dt_prueba <- dt[, .(ORIGIN_AIRPORT, DEPARTURE_DELAY)]

microbenchmark(
  base_R = aggregate(DEPARTURE_DELAY ~ ORIGIN_AIRPORT, data = df, FUN = mean, na.rm = TRUE),
  data_table = dt_prueba[, .(media = mean(DEPARTURE_DELAY, na.rm = TRUE)), by = ORIGIN_AIRPORT],
  times = 5
)


#######################################
# agrupación y resumen con data.table #
#######################################

library(data.table)
dt <- as.data.table(df)

# Media y desviación típica del retraso por aerolínea
dt[ AIR_TIME > 60, .(
  media = mean(ARRIVAL_DELAY, na.rm = TRUE),
  sd = sd(ARRIVAL_DELAY, na.rm = TRUE)
), by = AIRLINE]

# Top 3 destinos más frecuentes por origen
dt[, .N, by = .(ORIGIN_AIRPORT, DESTINATION_AIRPORT)][
  order(ORIGIN_AIRPORT, -N), head(.SD, 3), by = ORIGIN_AIRPORT]

# Vuelos con mayor retraso por día
dt[, .SD[which.max(ARRIVAL_DELAY)], by = .(YEAR, MONTH, DAY)]

# Proporción de vuelos con más de 60 min de retraso por destino
dt[, .(
  total = .N,
  prop_tarde = mean(ARRIVAL_DELAY > 60, na.rm = TRUE)
), by = DESTINATION_AIRPORT][order(-prop_tarde)]



########################
# Introducción a dplyr #
########################

# Cargamos el paquete dplyr
# install.packages("dplyr")
library(dplyr)

# Convertimos df en tibble para trabajar cómodamente (opcional)
df_tbl <- as_tibble(df)

# 1. SELECCIÓN DE FILAS: filter()

# Vuelos con más de 1 hora de retraso en llegada
df_tbl %>% 
  filter(ARRIVAL_DELAY > 60)

# Vuelos no cancelados con retraso en salida
df_tbl %>% 
  filter(CANCELLED == 0, DEPARTURE_DELAY > 15)

# 2. SELECCIÓN DE COLUMNAS: select()

# Seleccionamos solo columnas clave
df_tbl %>% 
  select(FLIGHT_NUMBER, ORIGIN_AIRPORT, DESTINATION_AIRPORT, ARRIVAL_DELAY)

# 3. ORDENAR FILAS: arrange()

# Vuelos con más retraso en llegada primero
df_tbl %>% 
  arrange(desc(ARRIVAL_DELAY))

# 4. NUEVAS VARIABLES: mutate()

# Creamos una columna con la ganancia de tiempo en vuelo
df_tbl %>% 
  mutate(GANANCIA_TIEMPO = DEPARTURE_DELAY - ARRIVAL_DELAY)

# Clasificamos los vuelos según distancia
df_tbl %>% 
  mutate(TIPO = if_else(DISTANCE < 500, "Corto", "Largo"))

# 5. RESUMEN DE DATOS: summarise() + group_by()

# Media de retraso por aerolínea
df_tbl %>% 
  group_by(AIRLINE) %>% 
  summarise(media_retraso = mean(ARRIVAL_DELAY, na.rm = TRUE))

# Porcentaje de cancelaciones por día de la semana
df_tbl %>% 
  group_by(DAY_OF_WEEK) %>% 
  summarise(prop_cancelados = mean(CANCELLED))

# 6. ENCADENAR OPERACIONES CON %>%

# Vuelos no cancelados, agrupados por origen, media de retraso en llegada
df_tbl %>% 
  filter(CANCELLED == 0) %>% 
  group_by(ORIGIN_AIRPORT) %>% 
  summarise(media_llegada = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>% 
  arrange(desc(media_llegada))

# 7. CONTEOS RÁPIDOS

# Número de vuelos por aerolínea
df_tbl %>% 
  count(AIRLINE, sort = TRUE)

# Número de vuelos por combinación de origen y destino
df_tbl %>% 
  count(ORIGIN_AIRPORT, DESTINATION_AIRPORT, sort = TRUE)00:06:01


########################
# Automatización dplyr #
########################

library(dplyr)

# Trabajamos con una versión en tibble del dataset
df_tbl <- as_tibble(df)

# 1. ENCADENAR OPERACIONES: flujo típico de análisis

# Análisis del retraso medio en llegada por aerolínea (solo vuelos no cancelados)
df_tbl %>%
  filter(CANCELLED == 0) %>%
  group_by(AIRLINE) %>%
  summarise(
    media_retraso = mean(ARRIVAL_DELAY, na.rm = TRUE),
    vuelos = n()
  ) %>%
  arrange(desc(media_retraso))

# 2. CREAR FUNCIONES REUTILIZABLES PARA ANÁLISIS

# Función para analizar el retraso medio por grupo
analiza_retraso <- function(data, variable_grupo) {
  data %>%
    filter(CANCELLED == 0) %>%
    group_by(across({{variable_grupo}})) %>%
    summarise(
      media_salida = mean(DEPARTURE_DELAY, na.rm = TRUE),
      media_llegada = mean(ARRIVAL_DELAY, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(media_llegada))
}

# Usamos la función para agrupar por aeropuerto de origen
analiza_retraso(df_tbl, ORIGIN_AIRPORT)

# Y también por día de la semana
analiza_retraso(df_tbl, DAY_OF_WEEK)


# 3. FLUJO COMPLETO DE ANÁLISIS CON PIPE

# Vuelos no cancelados, solo trayectos largos, clasificados por retraso
df_tbl %>%
  filter(CANCELLED == 0, DISTANCE > 1000) %>%
  mutate(
    CLASE_RETRASO = case_when(
      ARRIVAL_DELAY <= 0 ~ "A tiempo",
      ARRIVAL_DELAY <= 30 ~ "Leve",
      ARRIVAL_DELAY <= 120 ~ "Moderado",
      TRUE ~ "Grave"
    )
  ) %>%
  count(AIRLINE, CLASE_RETRASO) %>%
  group_by(AIRLINE) %>%
  mutate(porcentaje = n / sum(n)) %>%
  arrange(AIRLINE, desc(porcentaje))


# 4. PIPE DENTRO DE FUNCIONES

# Función para obtener los 3 destinos más frecuentes desde un aeropuerto
top_destinos <- function(data, aeropuerto_origen) {
  data %>%
    filter(ORIGIN_AIRPORT == aeropuerto_origen) %>%
    count(DESTINATION_AIRPORT, sort = TRUE) %>%
    slice_head(n = 3)
}

top_destinos(df_tbl, "JFK")



####################
# Fechas lubridate #
####################

# Cargamos el paquete lubridate
# install.packages("lubridate")
library(lubridate)

# Convertimos df a data.table
dt <- as.data.table(df)

# 1. CREAR COLUMNA DE FECHA A PARTIR DE AÑO, MES Y DÍA
dt[, FECHA := make_date(YEAR, MONTH, DAY)]

# 2. EXTRAER COMPONENTES DE LA FECHA
dt[, DIA_SEMANA := wday(FECHA, label = TRUE)]         # nombre del día
dt[, MES := month(FECHA, label = TRUE)]               # nombre del mes
dt[, ES_FIN_DE_SEMANA := wday(FECHA) %in% c(1, 7)]    # sábado o domingo

# 3. CONVERTIR HORA DE SALIDA PROGRAMADA EN HORA REAL

# SCHEDULED_DEPARTURE viene en formato HHMM (ej. 745 = 07:45)
dt[, HORA_PROG := SCHEDULED_DEPARTURE %/% 100]
dt[, MIN_PROG := SCHEDULED_DEPARTURE %% 100]

# Crear fecha-hora completa de salida programada
dt[, FECHA_HORA_PROG := make_datetime(YEAR, MONTH, DAY, HORA_PROG, MIN_PROG)]

# 4. HORA REAL DE SALIDA (DEPARTURE_TIME también es HHMM)
dt[, HORA_REAL := DEPARTURE_TIME %/% 100]
dt[, MIN_REAL := DEPARTURE_TIME %% 100]

# Fecha-hora real de salida
dt[, FECHA_HORA_REAL := make_datetime(YEAR, MONTH, DAY, HORA_REAL, MIN_REAL)]

# 5. DIFERENCIA ENTRE HORAS (en minutos)
dt[, DIF_HORA_SALIDA := as.numeric(difftime(FECHA_HORA_REAL, FECHA_HORA_PROG, units = "hours"))]

# 6. FILTRAR DATOS POR FECHA

# Vuelos del mes de enero
dt[month(FECHA) == 1]

# Vuelos de fin de semana con más de 1 hora de retraso en llegada
dt[ES_FIN_DE_SEMANA == TRUE & ARRIVAL_DELAY > 60]

# 7. FORMATEAR FECHAS (por ejemplo para gráficos)
dt[, FECHA_FORMATO := format(FECHA, "%d-%b-%Y")]  # 05-Jan-2015

# 8. VISUALIZACIÓN DE VUELOS POR DÍA
library(ggplot2)

vuelos_por_dia <- dt[, .N, by = FECHA]

ggplot(vuelos_por_dia, aes(x = FECHA, y = N)) +
  geom_line() +
  labs(title = "Número de vuelos por día", x = "Fecha", y = "Vuelos")

