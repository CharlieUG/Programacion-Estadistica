# Postwork Sesión 1.

#### Objetivo

El Postwork tiene como objetivo practicar los comandos básicos aprendidos durante la sesión, de tal modo que sirvan para reafirmar el conocimiento. 


#### Requisitos
- Concluir los retos
- Haber estudiado los ejemplos durante la sesión

#### Desarrollo

El siguiente postwork, servirá para ir desarrollando habilidades como si se tratara de un proyecto que evidencie el progreso del aprendizaje durante el módulo, sesión a sesión se irá desarrollando.

A continuación aparecen una serie de objetivos que se deben cumplir, es un ejemplo real de aplicación y tiene que ver con datos referentes a equipos de la liga española de fútbol (recuerda que los datos provienen siempre de diversas naturalezas), en este caso se cuenta con muchos datos que se pueden aprovechar, explotarlos y generar análisis interesantes que se pueden aplicar a otras áreas. Siendo así damos paso a las instrucciones: 

1. Descargar los datos de soccer de la temporada 2019/2020 de la primera división de la liga española: https://www.football-data.co.uk/spainm.php
<<<<<<< HEAD
- [SP1.csv](/SP1.csv)
```r

#Instalar los paquetes necesarios
install.packages("pheatmap")
library(pheatmap)

```
=======
- [SP1.csv](SP1.csv)
>>>>>>> 91f9d5ee751b6ea0a4b0897a45d9ed3468678aea
3. Importar los datos a R como un Dataframe
```r

sp1 <- read.csv("SP1.csv")
class(sp1)
dim(sp1)
sp1

```

2. Del dataframe que resulta de importar los datos a `R`, extraer las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG); guárdalos en vectores separados
```r

#Extracción de goles anotados en casa (FTHG):
golescasa <- sp1[,6]
golescasa

#Extracción de goles anotados como visitante (FTAG):
golesvis <- sp1[,7]
golesvis

```
3. Consultar cómo funciona la función `table` en `R`. Para ello, es posible ingresar los comandos `help("table")` o `?table` para leer la documentación.
```r
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

```
 
4. Responde a las siguientes preguntas:

a) ¿Cuántos goles tuvo el partido con mayor empate?
```r
#Interpretación 1: Resultado en empate que se repite mayor número de veces.
masempates <- max(resultados[1,1],resultados[2,2], resultados[3,3], resultados[4,4],
                  resultados[5,5], resultados[6,6])
masempates
# El mayor número de partidos empatados es 1-1 con 49 partidos.

#Interpretación 2: Resultado más alto en goles en empate.

mayorempate <- resultados[5,5]
mayorempate
#El partido con mayor número de goles en empate es 4-4 en un partido.
```

b) ¿En cuántos partidos ambos equipos empataron 0 a 0?
```r
empateceros <- resultados[1,1]
empateceros
# Hubo 33 partidos con empate 0-0.
```
c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar
que el equipo visitante (AG) metiera un solo gol?
```r
goleadalocal <- resultados[7,1]
goleadalocal
# En un partido, con resultado 6-0.
```
 
#### Ir al archivo de código fuente
- [PostWork 1](https://github.com/alsolisc/Postworks/tree/main/src/PostWork1.R)
