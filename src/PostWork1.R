#1. Del siguiente enlace, descarga los datos de soccer de la temporada 2019/2020 
#de la primera división de la liga española: 
#https://www.football-data.co.uk/spainm.php

install.packages("pheatmap")
library(pheatmap)
#2. Importa los datos a R como un Dataframe. 
#NOTA: No olvides cambiar tu dirección de trabajo a la ruta donde descargaste 
#tu archivo
sp1 <- read.csv("SP1.csv")
class(sp1)
dim(sp1)
sp1

#3. Del dataframe que resulta de importar los datos a `R`, extrae las columnas 
#que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG)
#y los goles anotados por los equipos que jugaron como visitante (FTAG); 
#guárdalos en vectores separado

#Extracción de goles anotados en casa (FTHG):
golescasa <- sp1[,6]
golescasa

#Extracción de goles anotados como visitante (FTAG):
golesvis <- sp1[,7]
golesvis

#4. Consulta cómo funciona la función `table` en `R`. Para ello, puedes ingresar
#los comandos `help("table")` o `?table` para leer la documentación.

#La función "Table" se utiliza para crear tablas de frecuencias de una vía. Los
#parámetros a utilizar son los nombres de los objetos sobre los que se quiere
#construir la tabla. Es muy útil para los vectores que se definieron en el 
#punto 3, pues nos permitiría obtener una tabla de doble entrada contabilizando
#la cantidad de partidos por cada resultado en número de goles.
#Fuente: Hernández, F. (2021) "Manual de R" alojado en 
#https://fhernanb.github.io/Manual-de-R/tablas.html

resultados <- table (golescasa, golesvis)
resultados

pheatmap(resultados, display_numbers = TRUE, number_color = "black",  show_rownames = TRUE,
         show_colnames = TRUE, main = "Partidos por resultado", cluster_rows = FALSE,
         cluster_cols = FALSE, color = hcl.colors(50, "BluYl"))

#5. Responde a las siguientes preguntas:

#  a) ¿Cuántos goles tuvo el partido con mayor empate?

#Interpretación 1: Resultado en empate que se repite mayor número de veces.
masempates <- max(resultados[1,1],resultados[2,2], resultados[3,3], resultados[4,4],
                  resultados[5,5], resultados[6,6])
masempates
# El mayor número de partidos empatados es 1-1 con 49 partidos.

#Interpretación 2: Resultado más alto en goles en empate.

mayorempate <- resultados[5,5]
mayorempate
#El partido con mayor número de goles en empate es 4-4 en un partido.


#  b) ¿En cuántos partidos ambos equipos empataron 0 a 0?

empateceros <- resultados[1,1]
empateceros
# Hubo 33 partidos con empate 0-0.

#  c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar
#que el equipo visitante (AG) metiera un solo gol?

goleadalocal <- resultados[7,1]
goleadalocal
# En un partido, con resultado 6-0.

