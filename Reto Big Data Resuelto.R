# EJERCICIO 1 – Visualización y limpieza básica con data.table
library(data.table)
df <- fread("C:/Users/linkedin/Dropbox/LinkedIn/titles.csv")
df[, DURACION := as.numeric(gsub("[^0-9]", "", duration))]
df[, DURACION_UNITS := gsub("[0-9]", "", duration)]

df[order(-DURACION)][1:5]

# EJERCICIO 2 – Filtrado eficiente
df[release_year > 2020 & DURACION > 2, .N, by = type]

# EJERCICIO 3 – Agrupación y resumen con data.table
df[!is.na(DURACION), .(Total = .N, Media = mean(DURACION)), by = .(type, country)][order(-Media)]

# Mejorado
df_expandido <- df[!is.na(country),
                   .(country = unlist(tstrsplit(country, ",\\s*")),
                     DURACION,
                     type)]

df_expandido[!is.na(DURACION),
             .(Total = .N, Media = mean(DURACION)),
             by = .(type, country)][order(-Media)]

# Calcular total y media por país y tipo
df_resultado <- df_expandido[!is.na(DURACION),
                             .(Total = .N, Media = mean(DURACION)),
                             by = .(type, country)][order(-Media)]

# EJERCICIO 4 – Clasificación personalizada
df[, CLASE_DURACION := fifelse(DURACION < 30, "Corto",
                               fifelse(DURACION <= 60, "Medio", "Largo"))]

# EJERCICIO 5 – Uso de funciones vectorizadas
df[, NUM_PALABRAS_TITULO := sapply(strsplit(title, " "), length)]
df[, NUM_PALABRAS_TITULO := NUM_PALABRAS_TITULO + 1]

# EJERCICIO 6 – Función personalizada + apply
clasifica_reciente <- function(año) ifelse(año > 2018, "Reciente", "Antiguo")
df[, CLASE_TIEMPO := sapply(release_year, clasifica_reciente)]

# EJERCICIO 7 – Agregación condicional
df[release_year >= 2015, .(DuracionMax = max(DURACION, na.rm = TRUE)), by = .(type, release_year)]

# EJERCICIO 8 – Join de datos
valoraciones <- data.table(title = c("Title1", "Title2", "Title3"), puntuacion = c(8.5, 7.9, 9.0))
df <- merge(df, valoraciones, by = "title", all.x = TRUE)

# EJERCICIO 9 – Transformación y eficiencia
object.size(df)
df[, `:=`(type = as.factor(type), country = as.factor(country), listed_in = as.factor(listed_in))]
object.size(df)

# EJERCICIO 10 – Análisis temporal
Sys.setlocale("LC_TIME", "C")
library(ggplot2)
df[,date_added]

df[, ANIO := as.IDate(as.Date(date_added, format = "%B %d, %Y"))]
estrenos_por_anio <- df[ANIO >= 2010, .N, by = ANIO]
ggplot(estrenos_por_anio, aes(x = ANIO, y = N)) + 
  geom_line() + 
  labs(title = "Títulos por año", x = "Año", y = "Cantidad")
