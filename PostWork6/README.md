#PostWork Sesión 6

#### Objetivo

- Desarrollar un modelo que permita realizar predicciones de una variable cuantitativa
 con base en variables predictoras.

####Requisitos

1. Tener instalado R y RStudio
2. Haber trabajado con el Prework y el Work

####Desarrollo

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
- [advertising.csv](/advertising.csv)

#### Ir al archivo de código fuente
- [PostWork 6](https://github.com/alsolisc/Postworks/tree/main/src/PostWork6.R)
