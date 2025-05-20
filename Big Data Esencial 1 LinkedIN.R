ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"

df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")

######################
# Evaluar Eficiencia #
######################

# Podemos usar proc.time()
inicio <- proc.time()
media <- mean(rnorm(1e7))
fin <- proc.time()
fin - inicio  # Tiempo total de ejecución


# Queremos calcular la media del retraso en la salida por aeropuerto de origen
inicio <- proc.time()
media_delay_loop <- numeric(length(unique(df$ORIGIN_AIRPORT)))
aeropuertos <- unique(df$ORIGIN_AIRPORT)

for (i in seq_along(aeropuertos)) {
  media_delay_loop[i] <- mean(df$DEPARTURE_DELAY[df$ORIGIN_AIRPORT == aeropuertos[i]], na.rm = TRUE)
}
names(media_delay_loop) <- aeropuertos
proc.time() - inicio  # Tiempo del bucle

# Lo mismo usando aggregate(), que es mucho más eficiente
inicio <- proc.time()
media_delay_agg <- aggregate(DEPARTURE_DELAY ~ ORIGIN_AIRPORT, data = df, FUN = mean, na.rm = TRUE)
proc.time() - inicio


# Ahora una función lenta de ejemplo que podemos optimizar
funcion_lenta <- function(n) {
  res <- numeric(n)
  for (i in 1:n) {
    res[i] <- sqrt(i^2 + 1)
  }
  return(res)
}

# Usamos Rprof para ver cuellos de botella
Rprof("perfil.out")
funcion_lenta(1e5)
Rprof(NULL)
summaryRprof("perfil.out")  # Analiza qué parte del código ha consumido más tiempo

# Otra herramienta útil: microbenchmark, ideal para comparar pequeñas funciones
#install.packages("microbenchmark")
library(microbenchmark)

microbenchmark(
  bucle = {
    res <- numeric(1000)
    for (i in 1:1000) res[i] <- sqrt(i)
  },
  vectorizado = sqrt(1:1000),
  times = 1000
)

# Para código más pesado, se puede usar parallel (veremos esto en otro script)

###############################
# Vectores, listas y matrices #
###############################

# VECTORES

# Un vector es una secuencia de elementos del mismo tipo
v_num <- c(10, 20, 30)        # Vector numérico
v_char <- c("a", "b", "c")    # Vector de caracteres
v_log <- c(TRUE, FALSE, TRUE) # Vector lógico

c(3,"a",TRUE)

# Podemos acceder a los elementos por posición
v_num[1]        # Primer elemento
v_char[2:3]     # Segundo y tercer elemento
v_num > 15

# Operaciones con vectores
v_num + 5       # Suma 5 a cada elemento
v_num * 2       # Multiplica cada valor por 2

# Concatenación de vectores
v_todo <- c(v_num, 100, 200)

# Uso en el dataset flights
ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")
aerolineas <- unique(df$AIRLINE)
head(aerolineas)  # Vector con nombres únicos de aerolíneas
df$AIRLINE

# MATRICES

# Una matriz tiene filas y columnas, todos los elementos del mismo tipo
m1 <- matrix(1:9, nrow = 3, ncol = 3)  # Matriz 3x3
m2 <- matrix(c("a", "b", "c", "d"), nrow = 2)

# Acceso a elementos
m1[1, 2]       # Fila 1, columna 2
m1[, 3]        # Tercera columna completa
m1[2, ]        # Segunda fila completa

# Operaciones con matrices
m3 <- matrix(1:9, nrow = 3)
m3 * 2         # Multiplica todos los valores por 2
m3 + m1        # Suma elemento a elemento

# Transponer la matriz
t(m1)

# Crear una matriz desde columnas del dataset flights
matriz_vuelos <- as.matrix(df[1:5, c("DEPARTURE_DELAY", "ARRIVAL_DELAY")])
matriz_vuelos


# LISTAS

# Una lista puede contener objetos de diferentes tipos
lista <- list(nombre = "Vuelo", retraso = 45, datos = c(1,2,3))

# Acceder a los elementos
lista$retraso
lista[[1]]      # Primer elemento
names(lista)    # Nombres de los elementos

# Modificar elementos
lista$retraso <- 60

# Añadir nuevos elementos
lista$nuevo <- "OK"

# Las listas permiten agrupar resultados complejos
media_y_sd <- list(
  media = mean(df$ARRIVAL_DELAY, na.rm = TRUE),
  sd = sd(df$ARRIVAL_DELAY, na.rm = TRUE)
)
media_y_sd

# Listas anidadas
lista_anidada <- list(a = 1:3, b = list(c = "x", d = "y"))
lista_anidada$b$d


# Comparación práctica

# Vectores: ideales para datos homogéneos, como retrasos o distancias
delays <- df$DEPARTURE_DELAY[1:5]

# Matrices: útiles para trabajar con datos numéricos tabulados
m_delays <- as.matrix(df[1:5, c("DEPARTURE_DELAY", "ARRIVAL_DELAY")])

# Listas: perfectas para guardar resultados heterogéneos o salidas de funciones
resumen <- list(
  origen = df$ORIGIN_AIRPORT[1],
  destino = df$DESTINATION_AIRPORT[1],
  distancia = df$DISTANCE[1]
)
resumen

##############
# DataFrames #
##############


# Creamos un data frame sencillo desde vectores
ciudades <- c("Madrid", "Barcelona", "Valencia")
habitantes <- c(3200000, 1600000, 800000)
coste_vida <- c(100, 95, 90)

df_ciudades <- data.frame(Ciudad = ciudades,
                          Habitantes = habitantes,
                          CosteVida = coste_vida)

df_ciudades[,"Ciudad"]

# Accedemos a columnas usando $
df_ciudades$Ciudad
df_ciudades$Habitantes

# También se puede acceder por índice o por nombre
df_ciudades[1, ]         # Primera fila
df_ciudades[, 2]         # Segunda columna
df_ciudades[1:2, "CosteVida"]


# Modificamos valores
df_ciudades$CosteVida <- df_ciudades$CosteVida + 5

# Añadimos una nueva columna
df_ciudades$Densidad <- c(5300, 16000, 5900)

# Añadimos una fila (con rbind)
nueva_fila <- data.frame(Ciudad = "Sevilla",
                         Habitantes = 690000,
                         CosteVida = 85,
                         Densidad = 5400)
df_ciudades <- rbind(df_ciudades, nueva_fila)


# Ahora trabajamos con el dataset flights
ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")

# Verificamos estructura y columnas
str(df)
names(df)

# Acceso a columnas
df$DEPARTURE_DELAY
df$AIRLINE[1:10]

# Seleccionamos varias columnas
df_sub <- df[, c("AIRLINE", "DEPARTURE_DELAY", "ARRIVAL_DELAY")]

# Seleccionamos las primeras filas
head(df_sub)
tail(df_sub)

# Filtrar datos: vuelos con más de 1h de retraso en llegada
retrasos_largos <- df[df$ARRIVAL_DELAY > 60,c("AIR_TIME")]

# Filtrar por múltiples condiciones
solo_no_cancelados <- df[df$CANCELLED == 0 | df$ARRIVAL_DELAY >= 30, ]

# Ordenar por retraso en salida (de mayor a menor)
ordenado <- df[order(-df$DEPARTURE_DELAY), ]

# Crear una nueva variable: diferencia entre retraso en salida y llegada
df$DIFERENCIA_RETRASO <- df$DEPARTURE_DELAY - df$ARRIVAL_DELAY

# Filtrar vuelos que salieron tarde pero llegaron a tiempo
compensaron_retraso <- df[df$DEPARTURE_DELAY > 0 & df$ARRIVAL_DELAY <= 0, ]

# Agrupaciones simples con aggregate
retraso_medio_por_aerolinea <- aggregate(ARRIVAL_DELAY ~ AIRLINE, data = df, FUN = mean, na.rm = TRUE)

# Otra agrupación: distancia media por aeropuerto de salida
distancia_media_origen <- aggregate(DISTANCE ~ ORIGIN_AIRPORT, data = df, FUN = mean)

# Cambiar nombres de columnas
names(df)[names(df) == "DEPARTURE_DELAY"] <- "RETRASO_SALIDA"

# Eliminar columnas
df$DIFERENCIA_RETRASO <- NULL

# Eliminar filas: quitamos vuelos cancelados
df <- df[df$CANCELLED == 0, ]


#############
# AGGREGATE #
#############

# La función aggregate() permite aplicar funciones a subconjuntos de datos agrupados

# Media de retraso en llegada por aerolínea

aggregate(df$DEPARTURE_DELAY, list(df$AIRLINE), mean, na.rm = TRUE)


















agg1 <- aggregate(ARRIVAL_DELAY ~ AIRLINE, data = df, FUN = mean, na.rm = TRUE)

# Suma total de retrasos en salida por aeropuerto de origen
agg2 <- aggregate(DEPARTURE_DELAY ~ ORIGIN_AIRPORT, data = df, FUN = sum, na.rm = TRUE)

# Número de vuelos por día
agg3 <- aggregate(FLIGHT_NUMBER ~ YEAR + MONTH + DAY, data = df, FUN = length)

# Número de vuelos por día de la semana
agg4 <- aggregate(FLIGHT_NUMBER ~ DAY_OF_WEEK, data = df, FUN = length)

# Media del retraso en llegada según si el vuelo fue cancelado o no
agg5 <- aggregate(ARRIVAL_DELAY ~ CANCELLED, data = df, FUN = mean, na.rm = TRUE)

# Media del tiempo total de vuelo por distancia (dividimos en grupos usando cut)
df$GRUPO_DISTANCIA <- cut(df$DISTANCE, breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf))

agg6 <- aggregate(AIR_TIME ~ GRUPO_DISTANCIA, data = df, FUN = mean, na.rm = TRUE)

# Agrupación por múltiples columnas
# Tiempo medio de vuelo por aerolínea y día de la semana
agg7 <- aggregate(AIR_TIME ~ AIRLINE + DAY_OF_WEEK, data = df, FUN = mean, na.rm = TRUE)

# Visualizamos algunos resultados
library(ggplot2)

ggplot(agg1, aes(x = AIRLINE, y = ARRIVAL_DELAY)) +
  geom_col() +
  labs(title = "Retraso medio en llegada por aerolínea", x = "Aerolínea", 
       y = "Retraso medio (min)")

ggplot(agg4, aes(x = factor(DAY_OF_WEEK), y = FLIGHT_NUMBER)) +
  geom_col() +
  labs(title = "Número de vuelos por día de la semana", x = "Día de la semana", 
       y = "Vuelos")

ggplot(agg6, aes(x = GRUPO_DISTANCIA, y = AIR_TIME)) +
  geom_col() +
  labs(title = "Tiempo medio de vuelo por grupo de distancia", x = "Distancia", 
       y = "Tiempo medio de vuelo (min)")

# También se puede usar aggregate() con funciones personalizadas
agg8 <- aggregate(ARRIVAL_DELAY ~ AIRLINE, data = df, 
                  FUN = function(x) mean(x[x > 0], na.rm = TRUE))  # solo retrasos positivos


####Ejemplo completo

# Nos aseguramos de no tener NAs en la columna necesaria
df_filtrado <- df[!is.na(df$DEPARTURE_DELAY), ]

# Creamos una función personalizada que devuelva varias métricas combinadas
resumen_retrasos <- function(x) {
  total <- length(x)
  n_retrasados <- sum(x > 0)
  media <- mean(x)
  prop_mayores_15 <- sum(x > 15) / total
  # Devolvemos los valores como un vector
  return(c(n_retrasados = n_retrasados,
           media_retraso = media,
           prop_15min = prop_mayores_15))
}

# Usamos aggregate con la función personalizada
agg_resumen <- aggregate(DEPARTURE_DELAY ~ ORIGIN_AIRPORT, data = df_filtrado, FUN = resumen_retrasos)

# El resultado es una columna con listas, que convertimos en varias columnas separadas
# Primero extraemos la matriz desde la columna de listas
valores <- do.call(rbind, list(agg_resumen$DEPARTURE_DELAY))

# Unimos las columnas al ORIGIN_AIRPORT
agg_final <- data.frame(ORIGIN_AIRPORT = agg_resumen$ORIGIN_AIRPORT, valores)

# Mostramos los resultados ordenados
agg_final <- agg_final[order(-agg_final$n_retrasados), ]
head(agg_final, 10)


##########################
# Programación vectorial #
##########################

# Evitar bucles innecesarios es clave para mejorar el rendimiento en R
# Vamos a comparar ejemplos con bucles y versiones vectorizadas

# Crear un vector con el doble de los números del 1 al 100000
# Versión con bucle
resultado_bucle <- numeric(100000)
for (i in 1:100000) {
  resultado_bucle[i] <- i * 2
}

# Versión vectorizada
resultado_vect <- (1:100000) * 2

# Verificamos que son iguales
all.equal(resultado_bucle, resultado_vect)


# Ejemplo realista con el dataset flights
# Queremos calcular la ganancia de tiempo: diferencia entre retraso en salida y retraso en llegada

# Con bucle
ganancia_bucle <- numeric(nrow(df))
for (i in 1:nrow(df)) {
  ganancia_bucle[i] <- df$DEPARTURE_DELAY[i] - df$ARRIVAL_DELAY[i]
}

# Con código vectorizado
ganancia_vect <- df$DEPARTURE_DELAY - df$ARRIVAL_DELAY

# Comprobamos si coinciden (puede haber diferencias por los NA)
sum(is.na(ganancia_vect))  # cuántos NA hay

# Creamos una nueva columna con la versión vectorizada
df$GANANCIA_TIEMPO <- ganancia_vect

# Otro ejemplo: vuelos puntuales (llegada con retraso <= 5 minutos)
df$PUNTUAL <- df$ARRIVAL_DELAY <= 5

# Contar número de vuelos puntuales
sum(df$PUNTUAL, na.rm = TRUE)

# Porcentaje de vuelos puntuales
mean(df$PUNTUAL, na.rm = TRUE)


# Clasificar vuelos según distancia (operación vectorizada con cut)
df$CATEGORIA_DISTANCIA <- cut(df$DISTANCE,
                              breaks = c(0, 500, 1000, 1500, 2000, Inf),
                              labels = c("Corta", "Media", "Larga", "Muy larga", "Extrema"))

# Ver la distribución
table(df$CATEGORIA_DISTANCIA)

# Calcular velocidad media de cada vuelo (DISTANCE / AIR_TIME)
df$VELOCIDAD_MEDIA <- df$DISTANCE / (df$AIR_TIME / 60)  # resultado en millas por hora

# Eliminar valores extremos (más de 800 mph o menos de 100)
df$VELOCIDAD_MEDIA[df$VELOCIDAD_MEDIA > 800 | df$VELOCIDAD_MEDIA < 100] <- NA


# Crear etiquetas según el nivel de retraso
df$CLASIFICACION_RETRASO <- ifelse(df$ARRIVAL_DELAY <= 0, "A tiempo",
                                   ifelse(df$ARRIVAL_DELAY <= 30, "Leve",
                                          ifelse(df$ARRIVAL_DELAY <= 120, "Moderado", "Grave")))

# Contamos cuántos hay de cada tipo
table(df$CLASIFICACION_RETRASO)

# Un resumen de cómo el vectorizado nos ahorra código:
# - No usamos bucles
# - Las operaciones se aplican a vectores completos
# - Se gana claridad y eficiencia
