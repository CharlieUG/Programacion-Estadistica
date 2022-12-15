#PostWork Sesión 6

#### Objetivo

- Desarrollar un modelo que permita realizar predicciones de una variable cuantitativa
 con base en variables predictoras.

#### Requisitos

1. Tener instalado R y RStudio
2. Haber trabajado con el Prework y el Work

#### Desarrollo

Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre 
como mejorar las ventas de un producto particular, y el conjunto de datos 
con el que disponemos son datos de publicidad que consisten en las ventas 
de aquel producto en 200 diferentes mercados, junto con presupuestos de 
publicidad para el producto en cada uno de aquellos mercados para tres 
medios de comunicación diferentes: TV, radio, y periódico. No es posible 
para nuestro cliente incrementar directamente las ventas del producto. Por 
otro lado, ellos pueden controlar el gasto en publicidad para cada uno de 
los tres medios de comunicación. Por lo tanto, si determinamos que hay una 
asociación entre publicidad y ventas, entonces podemos instruir a nuestro 
cliente para que ajuste los presupuestos de publicidad, y así 
indirectamente incrementar las ventas. 

En otras palabras, nuestro objetivo 
es desarrollar un modelo preciso que pueda ser usado para predecir las 
ventas sobre la base de los tres presupuestos de medios de comunicación. Ajustar 
modelos de regresión lineal múltiple a los datos advertisement.csv y elegir el 
modelo más adecuado siguiendo los procedimientos vistos


Considerar:

- Y: Sales (Ventas de un producto)
- X1: TV (Presupuesto de publicidad en TV para el producto)
- X2: Radio (Presupuesto de publicidad en Radio para el producto)
- X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)

```R
adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
```
#### 1. Importar datos
```R
adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
str(adv)
```
- [advertising.csv](/advertising.csv)

#### 2. Construir el modelo de regresión lineal múltiple completo
```R
modelo1 <- lm(Sales~., data = adv)
summary(modelo1)
```
>Con base en los resultados obtenidos, puede observarse que el coeficiente
>de regresión de la variable 'Newspaper' no puede considerarse estadísticamente 
>diferente de cero (valor p = 0.954) al nivel de significancia del 5%. 
>Por lo tanto, se procederá a eliminar dicho predictor.

#### 3. Construir un segundo modelo sin el predictor 'Newspaper'
```R
modelo2 <- update(modelo1,~.-Newspaper)
summary(modelo2)
```

>Según el resumen del modelo2, puede observarse que ahora los coeficientes de regresión
>de todas las variables son estadísticamente diferentes de cero con un nivel de
>significancia del 5%. Sin embargo, se contruirá un tercer modelo agregando un término
>de interacción entre las variables TV y Radio para determinar si el modelo mejora:"

#### 4. Construir un tercer modelo sin el predictor 'Newspaper' y con un término de interacción entre TV y Radio:"
```R
modelo3 <- update(modelo2,~.+TV:Radio)
summary(modelo3)
```

>Según el resumen del modelo3, puede observarse que ahora los coeficientes de regresión
>de todas las variables, así como de la interacción, son estadísticamente diferentes 
>de cero con un nivel de significancia del 5%. Por lo tanto, la ecuación de regresión lineal
>múltiple queda de la siguiente manera:"

>"y^ = 6.193 + 0.0436*TV + 0.0423*Radio + 0.0004*TV*Radio"

>"Asimismo, con base en el valor p de la prueba F (< 2.2e-16) puede concluirse que
>se rechaza la hipótesis nula y al menos un predictor o interacción se relaciona con las ventas
>de forma significativa. En seguida, se utilizarán algunos criterios estadísticos 
>para seleccionar el modelo definitivo. En primer lugar, se evaluará el R2 ajustado
>de los tres modelos:"

```R
summary(modelo1)$adj.r.squared
summary(modelo2)$adj.r.squared
summary(modelo3)$adj.r.squared
```

"El modelo3 tiene un R2 ajustado de 0.9127, el modelo2 uno de 0.9016, mientras que 
el del modelo1 fue de 0.9011, por lo que se concluye que el modelo3 tiene mayor capacidad 
explicativa. En este sentido, la variación en el modelo3 explica en un 91.27% la variación
en las ventas. En segundo lugar, se calculará el AIC y el BIC para los tres modelos:"

```R
AIC(modelo1,modelo2,modelo3)
BIC(modelo1,modelo2,modelo3)
```

"Según los resultados anteriores, los valores menores de AIC y BIC corresponden al modelo3,
por lo que éste se seleccionará como el mejor modelo."

"A continuación se hará la evaluación del ajuste del modelo comparando el modelo nulo 
con el definitivo (modelo3) mediante la prueba de razón de verosimilitud."

```R
anova(modelo3,update(modelo3,~1),test = "Chisq")
```

"Los resultados muestran que la devianza del modelo2 fue de 477.6 y la
del modelo nulo fue de 5556.0. Por lo tanto, la diferencia de devianza fue de 
5078.4 con 3 grados de libertad. El valor de p asociado con la prueba chi-cuadrada 
de razón de verosimilitud Pr(>chi) < 2.2e-16 indica que se rechaza la hipótesis nula. 
Por lo tanto, el modelo con los dos predictores y la interacción es significativo."

"A continuación, se interpretarán los resultados de los coeficientes de regresión:"

```R
modelo3$coefficients
```

"Para TV, se concluye que por cada incremento unitario en la inversión en TV, las
ventas aumentan en promedio 0.0436, siempre y cuando la inversión en Radio no cambie."
"Para Radio, se concluye que por cada incremento unitario en la inversión en Radio, las
ventas aumentan en promedio 0.0423, siempre y cuando la inversión en TV no cambie."
"Para la interacción puede interpretarse que un incremento unitario en TV se asocia con
un incremento promedio de 0.0436 + 0.0004*Radio en las ventas, y que un incremento unitario 
en Radio se asocia con un incremento promedio de 0.0423 + 0.0004*TV en las ventas"

#### Ir al archivo de código fuente
- [PostWork 6](https://github.com/alsolisc/Postworks/tree/main/src/PostWork6.R)
