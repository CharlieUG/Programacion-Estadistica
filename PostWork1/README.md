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
- [SP1.csv](/SP1.csv)
3. Importar los datos a R como un Dataframe

2. Del dataframe que resulta de importar los datos a `R`, extraer las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG); guárdalos en vectores separados

3. Consultar cómo funciona la función `table` en `R`. Para ello, es posible ingresar los comandos `help("table")` o `?table` para leer la documentación.
 
4. Responder a las siguientes preguntas:
  a) ¿Cuántos goles tuvo el partido con mayor empate?
  b) ¿En cuántos partidos ambos equipos empataron 0 a 0?
  c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?
 
#### Ir al archivo de código fuente
- [PostWork 1](https://github.com/alsolisc/Postworks/tree/main/src/PostWork1.R)
