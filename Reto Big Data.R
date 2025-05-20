# Desafío de analítica de datos en R
# Dataset: titles.csv
# Objetivo: aplicar técnicas de análisis y optimización con R usando data.table y funciones base
# Carga inicial
# library(data.table)
# df <- fread("titles.csv")

# EJERCICIO 1 – Visualización y limpieza básica con data.table
# Muestra las 5 filas con mayor duración. Asegúrate de que la columna de duración sea numérica.
# Si no lo es, limpia los valores y conviértela.

# EJERCICIO 2 – Filtrado eficiente
# Filtra solo los títulos publicados después de 2020 que duren más de 60 minutos.
# ¿Cuántos hay por tipo de contenido?

# EJERCICIO 3 – Agrupación y resumen con data.table
# Calcula el número total de títulos y la media de duración por tipo y país.
# Ordena de mayor a menor media.

# EJERCICIO 4 – Clasificación personalizada
# Crea una nueva columna categórica llamada `CLASE_DURACION`:
# - “Corto” si dura menos de 30 minutos
# - “Medio” si está entre 30 y 60
# - “Largo” si supera 60 minutos
# Usa `fifelse()` para crearla de forma eficiente.

# EJERCICIO 5 – Uso de funciones vectorizadas
# Crea un vector con el número de palabras en cada título.
# Hazlo sin bucles, usando `strsplit()` y `sapply()`.

# EJERCICIO 6 – Función personalizada + apply
# Crea una función que clasifique el contenido en:
# “Reciente” si es posterior a 2018, “Antiguo” si no.
# Aplícala a todo el dataset con `sapply()` o `vapply()` según convenga.

# EJERCICIO 7 – Agregación condicional
# ¿Cuál es la duración máxima de cada tipo de contenido para cada año desde 2015?
# Usa data.table con agrupación múltiple.

# EJERCICIO 8 – Join de datos
# Crea manualmente una tabla con valoraciones externas para 10 títulos.
# Une esta tabla al dataset original por título, usando `merge()` o join de data.table.

# EJERCICIO 9 – Transformación y eficiencia
# Convierte las columnas de tipo de contenido, país y categoría a `factor`.
# Calcula el tamaño del dataset antes y después de esta transformación.

# EJERCICIO 10 – Análisis temporal
# Extrae el año de la columna de fecha de publicación.
# Calcula cuántos títulos nuevos se estrenaron por año desde 2010.
# Representa esta evolución con un gráfico de líneas usando ggplot2.

# Fin del desafío
