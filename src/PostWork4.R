#POSTWORK SESION 4
#Utilizando la variable total_intl_charge de la base de datos telecom_service.csv 
#de la sesión 3, realiza un análisis probabilístico. Para ello, debes determinar
#la función de distribución de probabilidad que más se acerque el comportamiento
#de los datos. 
#Hint: Puedes apoyarte de medidas descriptivas o técnicas de visualización.

library(dplyr)
library(DescTools)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
df
summary(df)
dim(df)

#Medidas de tendencia central

Mode(df$total_intl_charge)
#La moda es de 2.7 con 62 repeticiones.

mean(df$total_intl_charge)
#La media es de 2.76

median(df$total_intl_charge)
#La mediana es 2.78


#Dado que la media, mediana y moda son muy cercanas, se puede suponer que la
#distribución es simétrica.

#Histograma
hist(df$total_intl_charge, prob=TRUE, xlab = "Total Intl Charge", ylab = "Density",
     main = "Histograma Total Intl Charge", col = "aquamarine3")

#Observando el histograma, podemos ajustar la distribución normal a los datos.

media <- mean(df$total_intl_charge)
media
std <- sd(df$total_intl_charge)
std

#La desviación estándar es de 0.7538.

#1. Grafica la distribución teórica de la variable aleatoria total_intl_charge
curve(dnorm(x, mean=media, sd=std), from=0, to=6,
  col= "aquamarine4", main ="Distribución teórica (normal)", ylab="f(x)",
  sub = expression(paste(mu==2.76, " y ", sigma==0.7538)))

#2. ¿Cuál es la probabilidad de que el total de cargos internacionales 
#sea menor a 1.85 usd?

pnorm(1.85,mean = media, sd=std, lower.tail = TRUE)

#La probabilidad de que el total de cargos internacionales sea menor a 1.85 usd
#es 11.25%.

#3. ¿Cuál es la probabilidad de que el total de cargos internacionales sea 
#mayor a 3 usd?

1-pnorm(3,mean=media, sd=std, lower.tail= TRUE)

#La probabilidad de que el total de cargos internaciones sea mayor a 3 usd
#es 37.74%.

#4. ¿Cuál es la probabilidad de que el total de cargos internacionales esté 
#entre 2.35usd y 4.85 usd?

pnorm(4.85, mean=media,sd=std, lower.tail = TRUE)-pnorm(2.35, mean=media, 
      sd=std, lower.tail = TRUE)

#La probabilidad de que el total de cargos internacionales esté entre 2.35 y 
#4.85 usd es del 70.60%

#5. Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales
#más alto que podría esperar?

qnorm(0.48, mean=media, sd=std)

#El total de cargos internacionales más alto es de 2.73 usd con una
#probabilidad del 48%.

#6. ¿Cuáles son los valores del total de cargos internacionales que dejan 
#exactamente al centro el 80% de probabilidad?

qnorm(0.1, mean=media, sd=std, lower.tail = TRUE); qnorm(0.9, mean=media, 
      sd=std, lower.tail = TRUE)

#Entre los cargos de 1.7986 usd y 3.7306 usd se presenta un 80% de probabilidad
#al centro de la distribución, es decir, entre esos dos valores se encuentra el 
#80% de los datos.

#GRÁFICAS

x <- seq(-4, 4, 0.01)*std + media
y <- dnorm(x, mean = media, sd = std)
integrate(dnorm, lower = x[1], upper = x[length(x)], mean = media, sd = std)
par(mfrow = c(2,3))

#1. Distribución normal
curve(dnorm(x, mean=media, sd=std), from=(0), to=(6),
      main ="Distribución teórica (normal)", ylab="f(x)",
      sub = expression(paste(mu==2.76, " y ", sigma==0.7538)))

#2. P(TIC<1.85) (TIC=total intl charge)
plot(x, y, type = "l", xlab = "x", ylab = "f(x)",
     sub = expression(paste("P(TIC<1.85)=0.1125")))
title(main = "P(TIC<1.85 usd)")
polygon(c(min(x), x[x<=1.85], 1.85), c(0, y[x<=1.85], 0), col="aquamarine1")

#3. P(TIC>3.0)
plot(x, y, type = "l", xlab = "x", ylab = "f(x)", 
     sub = expression(paste("P(TIC>3.0)=0.3774")))
title(main = "P(TIC>3.00 usd)")
polygon(c(3, x[x>3], max(x)), c(0, y[x>3], 0), col="aquamarine2")

#4. P(2.35<=TIC<=4.85)
plot(x, y, type = "l", xlab = "x", ylab = "f(x)", sub = expression(paste("P(2.35<=TIC<=4.85)=0.7060")))
title(main = "P(2.35<=TIC<=4.85 usd)")

polygon(c(2.35, x[x>=2.35 & x<=4.85], 4.85), c(0, y[x>=2.35 & x<=4.85], 0), 
        col="aquamarine3")

#5. P(TIC<=X)=0.48
plot(x, y, type = "l", xlab = "x", ylab = "f(x)", 
     sub = expression(paste("X=2.7268")))
title(main = "P(TIC<=X)=0.48")     
polygon(c(min(x), x[x<=2.7268], 2.7268), c(0, y[x<=2.7268], 0), col="aquamarine3")

#6.P(X1<=TIC<=x2)=0.80
plot(x, y, type = "l", xlab = "x", ylab = "f(x)", sub = expression(paste("X1=1.7986 y X2=3.7306")))
title(main = "(X1<=P(TIC)<=X2)=0.80")
polygon(c(1.7986, x[x>=1.7986 & x<=3.7306], 3.7306), 
        c(0, y[x>=1.7986 & x<=3.7306], 0), col="aquamarine4")

dev.off()
