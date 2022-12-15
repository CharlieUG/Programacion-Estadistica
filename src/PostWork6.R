# Postwork Sesión 06

#### Objetivo

#- Hacer estimaciones con regresión lineal.

#### Requisitos

#1. Tener instalado R, RStudio
#2. Haber trabajado con el Prework y el Work

# #### Desarrollo
# 
# Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre
# como mejorar las ventas de un producto particular, y el conjunto de datos
# con el que disponemos son datos de publicidad que consisten en las ventas
# de aquel producto en 200 diferentes mercados, junto con presupuestos de
# publicidad para el producto en cada uno de aquellos mercados para tres
# medios de comunicación diferentes: TV, radio, y periódico. No es posible
# para nuestro cliente incrementar directamente las ventas del producto. Por
# otro lado, ellos pueden controlar el gasto en publicidad para cada uno de
# los tres medios de comunicación. Por lo tanto, si determinamos que hay una
# asociación entre publicidad y ventas, entonces podemos instruir a nuestro
# cliente para que ajuste los presupuestos de publicidad, y así
# indirectamente incrementar las ventas.
# 
# En otras palabras, nuestro objetivo
# es desarrollar un modelo preciso que pueda ser usado para predecir las
# ventas sobre la base de los tres presupuestos de medios de comunicación. 
# Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y 
# elija el modelo más adecuado siguiendo los procedimientos vistos
# 
# Considera:
#   
# Y: Sales (Ventas de un producto)
# X1: TV (Presupuesto de publicidad en TV para el producto)
# X2: Radio (Presupuesto de publicidad en Radio para el producto)
# X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)


"1. Importar datos"

adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
str(adv)

"2. Construir el modelo de regresión lineal múltiple completo"

modelo1 <- lm(Sales~., data = adv)
summary(modelo1)

"Con base en los resultados obtenidos, puede observarse que el coeficiente
de regresión de la variable 'Newspaper' no puede considerarse estadísticamente 
diferente de cero (valor p = 0.954) al nivel de significancia del 5%. 
Por lo tanto, se procederá a eliminar dicho predictor."

"3. Construir un segundo modelo sin el predictor 'Newspaper'"

modelo2 <- update(modelo1,~.-Newspaper)
summary(modelo2)

"Según el resumen del modelo2, puede observarse que ahora los coeficientes de regresión
de todas las variables son estadísticamente diferentes de cero con un nivel de
significancia del 5%. Sin embargo, se contruirá un tercer modelo agregando un término
de interacción entre las variables TV y Radio para determinar si el modelo mejora:"

"4. Construir un tercer modelo sin el predictor 'Newspaper' y con un término de interacción
entre TV y Radio:"

modelo3 <- update(modelo2,~.+TV:Radio)
summary(modelo3)

"Según el resumen del modelo3, puede observarse que ahora los coeficientes de regresión
de todas las variables, así como de la interacción, son estadísticamente diferentes 
de cero con un nivel de significancia del 5%. Por lo tanto, la ecuación de regresión lineal
múltiple queda de la siguiente manera:"

"y^ = 6.193 + 0.0436*TV + 0.0423*Radio + 0.0004*TV*Radio"

"Asimismo, con base en el valor p de la prueba F (< 2.2e-16) puede concluirse que
se rechaza la hipótesis nula y al menos un predictor o interacción se relaciona con las ventas
de forma significativa. En seguida, se utilizarán algunos criterios estadísticos 
para seleccionar el modelo definitivo. En primer lugar, se evaluará el R2 ajustado
de los tres modelos:"

summary(modelo1)$adj.r.squared
summary(modelo2)$adj.r.squared
summary(modelo3)$adj.r.squared

"El modelo3 tiene un R2 ajustado de 0.9127, el modelo2 uno de 0.9016, mientras que 
el del modelo1 fue de 0.9011, por lo que se concluye que el modelo3 tiene mayor capacidad 
explicativa. En este sentido, la variación en el modelo3 explica en un 91.27% la variación
en las ventas. En segundo lugar, se calculará el AIC y el BIC para los tres modelos:"

AIC(modelo1,modelo2,modelo3)
BIC(modelo1,modelo2,modelo3)

"Según los resultados anteriores, los valores menores de AIC y BIC corresponden al modelo3,
por lo que éste se seleccionará como el mejor modelo."

"A continuación se hará la evaluación del ajuste del modelo comparando el modelo nulo 
con el definitivo (modelo3) mediante la prueba de razón de verosimilitud."

anova(modelo3,update(modelo3,~1),test = "Chisq")

"Los resultados muestran que la devianza del modelo2 fue de 477.6 y la
del modelo nulo fue de 5556.0. Por lo tanto, la diferencia de devianza fue de 
5078.4 con 3 grados de libertad. El valor de p asociado con la prueba chi-cuadrada 
de razón de verosimilitud Pr(>chi) < 2.2e-16 indica que se rechaza la hipótesis nula. 
Por lo tanto, el modelo con los dos predictores y la interacción es significativo."

"A continuación, se interpretarán los resultados de los coeficientes de regresión:"

modelo3$coefficients

"Para TV, se concluye que por cada incremento unitario en la inversión en TV, las
ventas aumentan en promedio 0.0436, siempre y cuando la inversión en Radio no cambie."
"Para Radio, se concluye que por cada incremento unitario en la inversión en Radio, las
ventas aumentan en promedio 0.0423, siempre y cuando la inversión en TV no cambie."
"Para la interacción puede interpretarse que un incremento unitario en TV se asocia con
un incremento promedio de 0.0436 + 0.0004*Radio en las ventas, y que un incremento unitario 
en Radio se asocia con un incremento promedio de 0.0423 + 0.0004*TV en las ventas"

"4. Validación de supuestos"

library(ggfortify)
autoplot(modelo3)

"1) Supuesto de linealidad: para verificar este supuesto se utiliza la gráfica
de residuales vs valores ajustes (esquina superior izquierda). Para el modelo3,
no se observa ningún patrón en el gráfico de residuales. Esto sugiere que puede
asumirse una relación lineal entre los predictores y la interacción con la variable
de respuesta."
"2) Supuesto de homoscedasticidad: para verificar este supuesto se utiliza la gráfica
de Scale-Location (esquina inferior izquierda). Para el modelo3, no se observa ningún
patrón en el gráfico. Esto sugiere que puede asumirse que se cumple el supuesto
de homogeneidad de varianzas."
"3) Supuesto de normalidad: para verificar este supuesto se utiliza la gráfica
Normal Q-Q (esquina superior derecha). Para el modelo3, se observa que la mayoría
de los puntos caen apróximadamente sobre la línea de referencia, de tal modo que
puede asumirse normalidad."
"4) Outliers: para verificar este supuesto se utiliza la gráfica Residuals vs
Leverage (esquina inferior derecha). Para el modelo3, se observan dos outliers 
(residuales 131 y 151), pues caen por debajo de -3 para el eje de los residuales
estandarizados."
"5) Valores apalancados: para verificar este supuesto se utiliza la gráfica Residuals vs
Leverage (esquina inferior derecha). Para detectar valores apalancados se utiliza
la fórmula 2(p+1)/n, la cual, para este caso dió un valor de 0.03, lo que significa 
que los residuos cuyo apalancamiento exceda de 0.03 pueden considerarse apalancados.
En este sentido, se concluye que existen algunos puntos con elevado apalancamiento
en los datos."
2*(2+1)/nrow(adv)
"6) Valores influyentes: para verificar este supuesto se utiliza la gráfica Residuals vs
Leverage (esquina inferior derecha). Para este caso, no se observan valores influyentes."

"Conclusión: se condujo un análisis de regresión lineal simple para estimar las ventas
a partir de tres variables predictoras. La variable dependiente fueron las ventas y
las variables independientes fueron la inversión en publicidad en televisión, radio
y periódicos. La prueba de razón de verosimilitud para el modelo con dos predictores
(televisión y radio) y la interacción entre televisión y radio (p < 0.001), indicó
que dicho modelo fue estadísticamente significativo. Al comparar el modelo con interacción
con los otros dos modelos, los valores de AIC y BIC (751.66 y 768.15, respectivamente)
indicaron que el modelo con las variables televisión y radio, y la interacción, era 
el que presentaba el mejor ajuste. El resumen del modelo mostró que tanto los predictores
individuales como la interacción resultaron estadísticamente significativos al nivel
de significancia del 5%. En este sentido, se sugiere al cliente que incremente los 
presupuestos de publicidad en televisión y radio, y que reduzca o elimine el presupuesto
en publicidad en periódicos para que pueda aumentar indirectamente las ventas." 
