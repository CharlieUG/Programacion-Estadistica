#POSTWORK SESIÓN 5
library(ggplot2)
#El data frame iris contiene información recolectada por Anderson sobre 50 flores 
#de 3 especies distintas (setosa, versicolor y virginica), incluyendo medidas en 
#centímetros del largo y ancho del sépalo así como de los pétalos.

#Estudios recientes sobre las mismas especies muestran que:
#1) En promedio, el largo del sépalo de la especie setosa (Sepal.Length) 
  #es igual a 5.7 cm
#2) En promedio, el ancho del pétalo de la especie virginica (Petal.Width) 
  #es menor a 2.1 cm
#3) En promedio, el largo del pétalo de la especie virginica es 1.1 cm 
  #más grande que el promedio del largo del pétalo de la especie versicolor.
#4) En promedio, no existe diferencia en el ancho del sépalo entre las 3 
  #especies.

#Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente 
#para concluir que los datos recolectados por Anderson están en línea con los nuevos 
#estudios. 

#Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el planteamiento 
#de hipótesis adecuada y concluye.

summary(iris)
iris$Species <- factor(iris$Species)

#1. HIPÓTESIS: En promedio, el largo del sépalo de la especie Setosa es 
    #de 5.7 cm.
#Ho: Promedio setosa Sepal.Lenght=5.7
#Ha: Promedio setosa Sepal.Lenght!=5.7

t.test(iris[iris$Species=="setosa", "Sepal.Length"], alternative="two.sided", 
       mu=5.7)

#NS=0.005, pvalue=2.2e-16
#Dado que pvalue<NS EEE para rechazar la hipótesis nula.

#2. HIPÓTESIS: En promedio, el ancho del pétalo de la especie virginica es 
    #menor a 2.1 cm.
#Ho:Promedio virginica Petal.With>=2.1
#Ha=Promedio virginica Petal.With<2.1

t.test(iris[iris$Species=="virginica", "Petal.Width"], alternative="less", 
       mu=2.1)

#NS=0.01, pvalue=0.03132
#Dado que pvalue>NS no EEE para rechazar la hipótesis nula.

#3. HIPÓTESIS: En promedio, el largo del pétalo de la especie virginica 
    #es 1.1 cm más grande que el promedio del largo del pétalo de la especie 
    #versicolor.

mean.virg <- mean(iris[iris$Species=="virginica", "Petal.Length"])
mean.versi <- mean(iris[iris$Species=="versicolor", "Petal.Length"])


#Ho: varianzas iguales
#Ha: varianzas diferentes

var.test(iris[iris$Species=="virginica", "Petal.Length"], iris[iris$Species=="versicolor", "Petal.Length"],
          ratio=1, alternative = "two.sided")

#NS=0.005, pvalue=0.2637
#Dado que pvalue>NS no EEE para rechazar la Ho, las varianzas son iguales.

#Ho: mean.virg-mean.versi >= 1.1
#Ha: mean.virg-mean.versi < 1.1

t.test(x = iris[iris$Species=="virginica", "Petal.Length"], y = iris[iris$Species=="versicolor", "Petal.Length"],
       alternative = "less", mu=1.1, var.equal=TRUE, conf.level=0.99)
#NS=0.01, pvalue=0.968
#Dado que p>NS no EEE para rechazar la Ho.

#4. HIPÓTESIS: En promedio, no existe diferencia en el ancho del sépalo entre las 3 
#especies.

#Análisis de varianza pues tenemos más de dos muestras.
#Ho: no hay diferencias entre las medias de Sepal.Width
#Ha: hay diferencias entre las medias de Sepal.Width

boxplot(Sepal.Width~Species, data = iris, col = "aquamarine3")

anova <- aov(Sepal.Width ~ Species, data=iris)
summary(anova)

