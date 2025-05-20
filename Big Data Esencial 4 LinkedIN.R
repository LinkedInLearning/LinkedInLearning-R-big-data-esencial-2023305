ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"

df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")

#########################
# Bigmemory y bigstatsr #
#########################


# Instalación de los paquetes si no están instalados
# install.packages("bigmemory")
# install.packages("bigstatsr")

library(bigmemory)
library(bigstatsr)
library(data.table)

# 0. CARGA DEL CSV ORIGINAL Y FILTRADO DE COLUMNAS NUMÉRICAS

# Leemos una muestra del CSV completo
df_raw <- fread(paste0(ruta,"flights.csv"))

# Filtramos solo columnas numéricas
num_cols <- names(df_raw)[sapply(df_raw, is.numeric)]
df_num <- df_raw[, ..num_cols]

# Guardamos un nuevo CSV solo con datos numéricos
write.table(df_num, paste0(ruta,"flights_numeric.csv"), sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

# 1. CREACIÓN DE UN big.matrix A PARTIR DEL CSV NUMÉRICO

# Solo columnas numéricas para que sea compatible con big.matrix

big_mat <- read.big.matrix(paste0(ruta,"flights_numeric.csv"),
                           header = TRUE,
                           type = "double",
                           backingfile = "flights.bin",
                           descriptorfile = "flights.desc")



# Podemos acceder a los datos como si fuera una matriz normal
big_mat[1:5, 1:4]

# 2. ESTADÍSTICAS BÁSICAS CON big.matrix

# Calcular la media de una columna
mean(big_mat[, 3], na.rm = TRUE)

# Calcular la media de cada columna
apply(big_mat[,], 2, mean, na.rm = TRUE)


# 3. USO DE bigstatsr PARA ANÁLISIS ESCALABLE


library(data.table)

# Leemos el CSV limpio con solo columnas numéricas
df <- fread(paste0(ruta,"flights_numeric.csv"))  # asegúrate de que está limpio (sin NA ni texto)
df$YEAR <- NULL

# Eliminamos filas con NA si las hubiera
df <- na.omit(df)

df <- df[,c("DEPARTURE_DELAY", "ARRIVAL_DELAY", "AIR_TIME", "DISTANCE" )]

library(bigstatsr)

# Convertimos el data.frame a una Filebacked Big Matrix
# Se guarda automáticamente en disco
X <- as_FBM(as.matrix(df), backingfile = "flights_fbm")

# Comprobamos que se ha guardado bien
dim(X)

# Estadísticas básicas por columna
big_colstats(X)


# SVD usando big_SVD
pca <- big_SVD(X, fun.scaling = big_scale(), k = 3)

# Visualización de los dos primeros componentes
plot(pca$u[1:1000, 1:2], col = "steelblue", pch = 20,
     main = "PCA con bigstatsr")


# Correlación
big_cor(X)[]



rm(X)
gc()

# CONCLUSIÓN:
# - `bigmemory` y `bigstatsr` permiten trabajar con grandes volúmenes de datos sin cargarlos en RAM.
# - Son ideales para análisis estadísticos básicos o modelos que requieren matrices grandes.
# - `bigstatsr` permite además hacer PCA y escalados de forma paralela y optimizada.

###################
# disk.frame y ff #
###################

# Paquetes necesarios
# https://cran.rstudio.com/bin/windows/Rtools/
# install.packages("")
# install.packages("disk.frame")
# install.packages("ff")
library(disk.frame)
library(ff)
library(data.table)

# 1. USO DE `disk.frame`: particiona y trabaja desde disco
setup_disk.frame()  # inicializa los recursos de disk.frame

# Leemos un CSV grande como disk.frame (requiere que exista en disco)
# Solo de ejemplo, con columnas mixtas
df_disk <- csv_to_disk.frame("flights.csv", outdir = "flights_df", overwrite = TRUE)

# Vemos estructura
df_disk

# Podemos aplicar operaciones por partes 
library(dplyr)

df_disk %>%
  filter(ARRIVAL_DELAY > 60) %>%
  summarise(media_retraso = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>% collect()

# Agrupación por aerolínea
df_disk %>%
  group_by(AIRLINE) %>%
  summarise(
    retraso_medio = mean(ARRIVAL_DELAY, na.rm = TRUE),
    vuelos = n()
  ) %>%
  collect() %>%        # une las particiones en un solo resultado
  arrange(desc(retraso_medio))


# 2. USO DE `ff`: matrices y vectores grandes almacenados en disco

# Creamos una matriz ff de ejemplo
mat_ff <- ff(vmode = "double", dim = c(1e6, 5))  # 1 millón de filas, 5 columnas

# Asignamos valores por partes (sin cargar todo en RAM)
mat_ff[ , 1] <- rnorm(1e6)
mat_ff[ , 2] <- runif(1e6)

# Operaciones básicas: media por columna
apply(mat_ff[,], 2, mean)

# Visualización de un subconjunto
mat_ff[1:5, 1:3]

# Guardar objeto en disco
ffsave(mat_ff, file = "mi_matriz_ff")

# Para cargarlo después:
# ffload("mi_matriz_ff")

# 3. CUÁNDO USAR CADA UNO

# - disk.frame: ideal para CSVs grandes con columnas mixtas (numéricas, texto, fechas).
#   Permite agrupar, filtrar y resumir con sintaxis tipo dplyr sin cargar todo en RAM.

# - ff: ideal para trabajo numérico tipo matriz (regresión, simulaciones).
#   Más bajo nivel, pero muy eficiente si sabes exactamente lo que quieres.

# CONCLUSIÓN:
# Estas herramientas permiten analizar datos más grandes que la memoria disponible,
# sin necesidad de clústeres o bases de datos externas.

########################
# foreach y doParallel #
########################

# Instalación de paquetes si no los tienes
# install.packages("foreach")
# install.packages("doParallel")

library(foreach)
library(doParallel)

# 1. DETECTAR NÚCLEOS Y REGISTRAR CLÚSTER

# Detectamos los núcleos disponibles
num_cores <- parallel::detectCores()

# Creamos un clúster dejando 1 núcleo libre
cl <- makeCluster(num_cores - 1)

# Registramos el clúster para foreach
registerDoParallel(cl)

# 2. EJEMPLO SIMPLE: calcular raíces cuadradas en paralelo

# Creamos un vector de prueba
valores <- 1:1e5

# Versión secuencial con for
resultado_seq <- numeric(length(valores))
for (i in seq_along(valores)) {
  resultado_seq[i] <- sqrt(valores[i])
}

c(1,2,3)

# Versión paralela con foreach
resultado_par <- foreach(i = valores, .combine = c) %dopar% {
  sqrt(i)
}

# Comprobamos que son iguales
all.equal(resultado_seq, resultado_par)

# 3. EJEMPLO REALISTA CON EL DATASET flights

# Cargamos y preparamos los datos
library(data.table)
library(foreach)
library(doParallel)
library(microbenchmark)

ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"

df <- read.csv(paste0(ruta,"flights_small.csv"), sep = ";")

# Queremos calcular la media de ARRIVAL_DELAY por cada aerolínea en paralelo


# Preparamos los datos
df_df <- na.omit(as.data.frame(df[, c("AIRLINE", "ARRIVAL_DELAY")]))
df_dt <- as.data.table(df_df)

# Medimos tiempo con aggregate()
tiempo_agg <- microbenchmark(
  aggregate(ARRIVAL_DELAY ~ AIRLINE, data = df_df, FUN = mean),
  times = 3
)

# Configuramos clúster para foreach
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# Medimos tiempo con foreach en paralelo
tiempo_foreach <- microbenchmark(
  foreach(a = unique(df_dt$AIRLINE), .combine = rbind, .packages = "data.table") %dopar% {
    data.table(AIRLINE = a, MEDIA = df_dt[AIRLINE == a, mean(ARRIVAL_DELAY)])
  },
  times = 3
)

stopCluster(cl)

# Mostramos tiempos
print(tiempo_agg)
print(tiempo_foreach)


#########
# BIGLM #
#########

# Paquetes necesarios
# install.packages("biglm")
# install.packages("data.table")

library(biglm)
library(data.table)

# Preparamos un dataset simulado tipo flights (con muchas filas)
df <- read.csv("flights.csv")
df <- as.data.table(df[,c("ARRIVAL_DELAY", "DISTANCE" , "AIR_TIME" , "DEPARTURE_DELAY")])
n <- nrow(df)


# Queremos predecir ARRIVAL_DELAY a partir de DISTANCE, AIR_TIME y DEPARTURE_DELAY

# 1. CONVERTIMOS A CHUNKS: por ejemplo en bloques de 100.000 filas
chunk_size <- 1e5
chunks <- split(1:n, ceiling(seq_along(1:n) / chunk_size))

# 2. Ajustamos el modelo con biglm en streaming

# Creamos fórmula
f <- ARRIVAL_DELAY ~ DISTANCE + AIR_TIME + DEPARTURE_DELAY

# Modelo inicial con el primer bloque
modelo <- biglm(f, data = df[chunks[[1]]])

# Actualizamos el modelo con los siguientes bloques
for (i in 2:length(chunks)) {
  modelo <- update(modelo, df[chunks[[i]]])
}

# 3. Resultados del modelo
summary(modelo) # lm

summary(modelo)$rsq

predict(modelo, df[1:10])


# CONCLUSIÓN:
# - `biglm` permite ajustar modelos lineales sin cargar todo el dataset a memoria.
# - Es ideal para análisis por partes o con streams de datos.


##########
# SparkR #
##########

# Iniciar sesión SparkR
# install.packages("SparkR")
library(SparkR)
sparkR.session(appName = "Análisis vuelos", master = "local[*]")

# Leer un CSV grande como Spark DataFrame
df_spark <- read.df("flights_big.csv", source = "csv", header = TRUE, inferSchema = TRUE)

# Ver esquema del DataFrame
printSchema(df_spark)

# Seleccionar columnas
select(df_spark, "AIRLINE", "ARRIVAL_DELAY")

# Filtrar vuelos con más de 1h de retraso
retrasados <- filter(df_spark, df_spark$ARRIVAL_DELAY > 60)

# Agrupar por aerolínea y calcular retraso medio
retraso_medio <- df_spark %>%
  groupBy("AIRLINE") %>%
  agg(mean(df_spark$ARRIVAL_DELAY))

# Ordenar por mayor retraso
ordenado <- arrange(retraso_medio, desc(retraso_medio$`avg(ARRIVAL_DELAY)`))

# Crear nueva columna: vuelo puntual o no
df_spark <- withColumn(df_spark, "PUNTUAL", df_spark$ARRIVAL_DELAY <= 5)

# Contar vuelos por día de la semana
vuelos_dia <- df_spark %>%
  groupBy("DAY_OF_WEEK") %>%
  count()

# Estadísticas resumen de ARRIVAL_DELAY
describe(df_spark, "ARRIVAL_DELAY")

# Conversión a SparkR DataFrame desde R data.frame
df_r <- data.frame(x = 1:100, y = rnorm(100))
df_spark_r <- createDataFrame(df_r)

# Exportar resultados como CSV
write.df(ordenado, path = "salida/retrasos_por_aerolinea", source = "csv", mode = "overwrite")

# Finalizar sesión Spark
sparkR.session.stop()


