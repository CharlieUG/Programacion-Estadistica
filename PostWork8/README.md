# Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México

#### OBJETIVO

- Realizar un análisis estadístico completo de un caso 
- Publicar en un repositorio de Github el análisis y el código empleado 

#### REQUISITOS

- Haber realizado los works y postworks previos 
- Tener una cuenta en Github o en RStudioCloud

#### DESARROLLO

Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene recursos financieros extrar al ingreso y en si presenta o no inseguridad alimentaria. Además, está interesado en un modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto Nacional de Salud Pública en México. La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:
- nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
- area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
- numpeho (Número de persona en el hogar)
- refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
- edadjef (Edad del jefe/a de familia)
- sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
- añosedu (Años de educación del jefe de familia)
- ln_als (Logarítmo natural del gasto en alimentos saludables)
- ln_alns (Logarítmo natural del gasto en alimentos no saludables)
- IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"

```R
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
```

1) Plantear el problema del caso
2) Realizar un análisis descriptivo de la información
3) Calcular probabilidades que nos permitan entender el problema en México
4) Plantear hipótesis estadísticas y concluye sobre ellas para entender el problema en México
5) Estimar un modelo de regresión, lineal o logístico, para identificiar los determinanres de la inseguridad alimentaria en México
6) Escribir el análisis en un archivo README.MD y el código en un script de R y publicar ambos en un repositorio de Github.

NOTA: Todo el planteamiento deberá estár correctamente desarrollado y se deberá analizar e interpretar todos los resultados para poder dar una conclusión final al problema planteado.

#### Ir al archivo de código fuente
- [PostWork 8](src/PostWork8.R)
