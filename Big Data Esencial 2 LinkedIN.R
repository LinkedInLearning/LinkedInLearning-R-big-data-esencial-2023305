
ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"

df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")

###################
# Funciones Apply #
###################

# El uso de bucles for en R puede ser más lento comparado con funciones de la familia apply
# Vamos a ver ejemplos comparando ambos enfoques

# EJEMPLO 1 – Calcular la media por fila en una matriz

# Creamos una matriz de ejemplo
m <- matrix(1:20, nrow = 5, ncol = 4)
m

apply(m,1,sum)

# Con bucle
medias_filas_bucle <- numeric(nrow(m))
for (i in 1:nrow(m)) {
  medias_filas_bucle[i] <- mean(m[i, ])
}

# Con apply (más directo y eficiente)
medias_filas_apply <- apply(m, 1, mean)

# Comprobamos si son iguales
all.equal(medias_filas_bucle, medias_filas_apply)


# EJEMPLO 2 – Calcular retraso medio por fila de vuelos (salida y llegada)
# Para un subconjunto del dataframe
df_subset <- df[1:1000, c("DEPARTURE_DELAY", "ARRIVAL_DELAY")]

# Con bucle
media_por_vuelo_bucle <- numeric(nrow(df_subset))
for (i in 1:nrow(df_subset)) {
  media_por_vuelo_bucle[i] <- mean(as.numeric(df_subset[i, ]), na.rm = TRUE)
}

# Con apply
media_por_vuelo_apply <- apply(df_subset, 1, function(x) mean(as.numeric(x), na.rm = TRUE))


# EJEMPLO 3 – Sumar valores por columna

# Bucle
suma_columnas_bucle <- numeric(ncol(df_subset))
for (j in 1:ncol(df_subset)) {
  suma_columnas_bucle[j] <- sum(df_subset[, j], na.rm = TRUE)
}

# Con apply (margen 2 = columnas)
suma_columnas_apply <- apply(df_subset, 2, function(x) sum(x, na.rm = TRUE))



# CONCLUSIÓN
# - Los bucles funcionan pero son más lentos y el código es más largo
# - apply() es más claro, más limpio y vectorizado internamente
# - apply() se usa mucho en matrices y data frames numéricos


###################
# Variantes Apply #
###################

# apply() se usa para aplicar funciones a filas o columnas de matrices o data frames numéricos

# Creamos una matriz de ejemplo
mat <- matrix(1:12, nrow = 4)

# Media por fila
apply(mat, 1, mean)

# Suma por columna
apply(mat, 2, sum)


# lapply() devuelve una lista con los resultados
# Ideal para aplicar funciones a cada elemento de una lista o vector

# Nombres de columnas del dataset
columnas_numericas <- c("DEPARTURE_DELAY", "ARRIVAL_DELAY", "DISTANCE", "AIR_TIME")

columnas_numericas <- colnames(df)[sapply(df, is.numeric)]

# Obtenemos medias con lapply()
medias_lapply <- lapply(df[, columnas_numericas], mean, na.rm = TRUE)

# Resultado es una lista
medias_lapply

# sapply() intenta simplificar el resultado (lo devuelve como vector si puede)
medias_sapply <- sapply(df[, columnas_numericas], mean, na.rm = TRUE)

# Resultado como vector con nombres
medias_sapply


# tapply() permite aplicar una función agrupando por una variable
# Por ejemplo, media del retraso en salida por aerolínea
tapply(df$DEPARTURE_DELAY, df$AIRLINE, mean, na.rm = TRUE)

# Otro ejemplo: número de vuelos por aeropuerto de origen
tapply(df$FLIGHT_NUMBER, df$ORIGIN_AIRPORT, length)

# Proporción de vuelos cancelados por día de la semana
tapply(df$CANCELLED, df$DAY_OF_WEEK, mean)

# Podemos almacenar el resultado y ordenarlo
retraso_aerolinea <- tapply(df$ARRIVAL_DELAY, df$AIRLINE, mean, na.rm = TRUE)
retraso_ordenado <- sort(retraso_aerolinea, decreasing = TRUE)

# Mostramos los que más retraso medio tienen
head(retraso_ordenado, 5)


# Comparamos formas de aplicar funciones:
# apply → para filas o columnas de estructuras rectangulares
# lapply → lista, devuelve lista
# sapply → lista o vector, devuelve vector o matriz si puede
# tapply → un vector agrupado por otro vector

# EJEMPLO REALISTA: calcular estadísticas básicas por aerolínea
stats_por_aerolinea <- tapply(df$ARRIVAL_DELAY, df$AIRLINE, function(x) {
  c(media = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    maximo = max(x, na.rm = TRUE))
})

# Este resultado es una lista con vectores nombrados
stats_por_aerolinea[["AA"]]  # Estadísticas para la aerolínea AA


##################################
# Funciones personalizadas apply #
##################################

# Vamos a crear nuestras propias funciones para usarlas con apply, lapply, sapply o tapply

# EJEMPLO 1 – Usamos apply() con una función que calcula la desviación absoluta media

# Creamos una matriz de ejemplo
set.seed(1)
matriz <- matrix(rnorm(20), nrow = 5)

# Función personalizada: desviación absoluta media
desviacion_abs_media <- function(x) {
  mean(abs(x - mean(x)))
}

# Aplicamos la función a cada fila
apply(matriz, 1, desviacion_abs_media)

# Y a cada columna
apply(matriz, 2, desviacion_abs_media)


# EJEMPLO 2 – Función para clasificar nivel de retraso por fila en el dataset
# Usamos apply sobre data frame

# Subconjunto con columnas de retraso
df_retrasos <- df[, c("DEPARTURE_DELAY", "ARRIVAL_DELAY")]

# Creamos la función
clasifica_retraso <- function(fila) {
  media <- mean(as.numeric(fila), na.rm = TRUE)
  if (is.na(media)) return(NA)
  if (media <= 0) {
    return("Sin retraso")
  } else if (media <= 30) {
    return("Retraso leve")
  } else if (media <= 120) {
    return("Retraso medio")
  } else {
    return("Retraso grave")
  }
}

# Aplicamos la función por fila (margen 1)
df$NIVEL_RETRASO <- apply(df_retrasos, 1, clasifica_retraso)

# Vemos los resultados
table(df$NIVEL_RETRASO)


# EJEMPLO 3 – Crear función para sapply que devuelve texto según distancia

# Función personalizada
clasifica_distancia <- function(x) {
  if (is.na(x)) return(NA)
  if (x < 500) return("Corta")
  if (x < 1000) return("Media")
  if (x < 2000) return("Larga")
  return("Muy larga")
}

# Aplicamos con sapply
df$TIPO_DISTANCIA <- sapply(df$DISTANCE, clasifica_distancia)

# Ver distribución
table(df$TIPO_DISTANCIA)


# EJEMPLO 4 – tapply con función personalizada para calcular desviación típica solo en retrasos positivos

# Función
sd_positiva <- function(x) {
  x_pos <- x[x > 0]
  if (length(x_pos) == 0) return(NA)
  return(sd(x_pos))
}

# Aplicamos por aerolínea
sd_por_aerolinea <- tapply(df$ARRIVAL_DELAY, df$AIRLINE, sd_positiva)

# Mostramos resultados ordenados
sort(sd_por_aerolinea, decreasing = TRUE)


# EJEMPLO 5 – lapply para obtener múltiples estadísticas por variable

# Lista de columnas numéricas
cols <- c("DEPARTURE_DELAY", "ARRIVAL_DELAY", "AIR_TIME", "DISTANCE")

# Función que devuelve varias estadísticas
resumen <- function(x) {
  c(media = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    maximo = max(x, na.rm = TRUE),
    minimo = min(x, na.rm = TRUE))
}

# Aplicamos con lapply
stats <- lapply(df[, cols], resumen)

# Accedemos a los resultados
stats[["AIR_TIME"]]

############
# parApply #
############

# parApply pertenece al paquete parallel y funciona como apply pero en paralelo

# Cargamos el paquete
# install.packages("parallel")
library(parallel)

# Creamos una matriz para el ejemplo
set.seed(123)
mat <- matrix(rnorm(1e6), nrow = 1000, ncol = 1000)

# Número de núcleos disponibles
num_cores <- detectCores()

# Creamos un clúster
cl <- makeCluster(num_cores - 1)  # dejamos 1 libre

# Exportamos variables al clúster si es necesario
clusterExport(cl, varlist = c("mat"))

# Aplicamos una función por fila en paralelo
resultado_paralelo <- parApply(cl, mat, 1, function(x) mean(abs(x)))

# Comparamos con apply normal
resultado_secuencial <- apply(mat, 1, function(x) mean(abs(x)))

# Verificamos si los resultados coinciden
all.equal(resultado_paralelo, resultado_secuencial)

# Cerramos el clúster
stopCluster(cl)


# EJEMPLO CON FLIGHTS – calcular el retraso medio por fila (salida y llegada)

# Subconjunto limpio del dataset
df_sub <- df[!is.na(df$DEPARTURE_DELAY) & !is.na(df$ARRIVAL_DELAY), c("DEPARTURE_DELAY", "ARRIVAL_DELAY")]

# Creamos una función personalizada
media_retraso <- function(fila) {
  mean(as.numeric(fila))
}

# Creamos un nuevo clúster
cl <- makeCluster(num_cores - 1)

# Exportamos variables necesarias
clusterExport(cl, varlist = c("df_sub", "media_retraso"))

# Aplicamos parApply por fila
retraso_medio_par <- parApply(cl, df_sub, 1, media_retraso)

# Comparamos con apply clásico
retraso_medio_normal <- apply(df_sub, 1, media_retraso)
all.equal(retraso_medio_par, retraso_medio_normal)

# Añadimos al dataframe original
df$RETRASO_MEDIO_PAR <- NA
df$RETRASO_MEDIO_PAR[!is.na(df$DEPARTURE_DELAY) & !is.na(df$ARRIVAL_DELAY)] <- retraso_medio_par

# Cerramos clúster
stopCluster(cl)


###### Comparativa

# Usamos solo las primeras 100000 filas para que sea rápido pero significativo

ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights.csv"))

df_test <- df[, c("DEPARTURE_DELAY", "ARRIVAL_DELAY")]

# Versión secuencial con apply
tiempo_apply <- system.time({
  retraso_medio_apply <- apply(df_test, 1, function(x) mean(as.numeric(x)))
})

# Versión paralela con parApply
num_cores <- detectCores()
cl <- makeCluster(num_cores - 1)

clusterExport(cl, varlist = c("df_test"))

tiempo_parapply <- system.time({
  retraso_medio_parapply <- parApply(cl, df_test, 1, function(x) mean(as.numeric(x)))
})

stopCluster(cl)

# Mostramos tiempos comparativos
tiempo_apply
tiempo_parapply

# Comprobamos si los resultados son iguales
all.equal(retraso_medio_apply, retraso_medio_parapply)

# Diferencia de tiempo
tiempo_apply["elapsed"] - tiempo_parapply["elapsed"]


# CONCLUSIÓN:
# - parApply es útil cuando se aplican funciones pesadas a muchas filas o columnas
# - La mejora se nota especialmente con grandes volúmenes de datos
# - Requiere gestionar el clúster (crear, exportar variables, cerrar)

##########################
# Ejemplo completo apply #
##########################

ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")

# Subconjunto de columnas numéricas
vars <- c("DEPARTURE_DELAY", "ARRIVAL_DELAY", "AIR_TIME", "DISTANCE")

# 1. Obtener resumen estadístico de columnas numéricas con lapply
resumen_variables <- lapply(df[, vars], function(x) {
  c(media = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    desviacion = sd(x, na.rm = TRUE),
    maximo = max(x, na.rm = TRUE),
    minimo = min(x, na.rm = TRUE))
})

# Convertimos a data frame para visualizar mejor
resumen_df <- as.data.frame(do.call(rbind, resumen_variables))
resumen_df


# 2. Usamos apply para clasificar filas por nivel de puntualidad
df_sub <- df[, c("DEPARTURE_DELAY", "ARRIVAL_DELAY")]

df$CATEGORIA_PUNTUALIDAD <- apply(df_sub, 1, function(fila) {
  if (all(is.na(fila))) return(NA)
  media <- mean(fila, na.rm = TRUE)
  if (media <= 0) return("A tiempo")
  if (media <= 30) return("Leve")
  if (media <= 120) return("Moderado")
  return("Grave")
})

# Vemos la distribución
table(df$CATEGORIA_PUNTUALIDAD)

# 3. sapply para calcular proporciones de vuelos cancelados por día
prop_cancelados <- sapply(split(df$CANCELLED, df$DAY_OF_WEEK), function(x) mean(x, na.rm = TRUE))
prop_cancelados

# 4. tapply para analizar distancia media por aerolínea
dist_media <- tapply(df$DISTANCE, df$AIRLINE, mean, na.rm = TRUE) # aggregate
dist_media_ordenada <- sort(dist_media, decreasing = TRUE)
head(dist_media_ordenada, 5)

# 5. tapply con función personalizada que devuelve varias estadísticas
resumen_retrasos <- tapply(df$ARRIVAL_DELAY, df$AIRLINE, function(x) {
  c(media = mean(x, na.rm = TRUE),
    maximo = max(x, na.rm = TRUE),
    positivos = sum(x > 0, na.rm = TRUE),
    proporcion_graves = mean(x > 60, na.rm = TRUE))
})

# Convertimos la lista a matriz
resumen_retrasos_mat <- do.call(rbind, resumen_retrasos)
head(resumen_retrasos_mat)

# 6. Usamos apply sobre la matriz para normalizar columnas (estandarizar)
matriz_retrasos <- resumen_retrasos_mat[, c("media", "maximo")]
matriz_norm <- apply(matriz_retrasos, 2, function(x) (x - mean(x)) / sd(x))

# Convertimos en data frame con nombres de aerolínea
df_norm <- data.frame(AIRLINE = rownames(matriz_norm), matriz_norm)
df_norm

# CONCLUSIÓN:
# - La familia apply permite realizar análisis complejos sin bucles
# - Muy útil para trabajar con funciones personalizadas
# - Eficiencia y claridad, incluso en datasets grandes


