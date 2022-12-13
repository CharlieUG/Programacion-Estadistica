## Postwork 7. Predicciones de la temperatura global

### OBJETIVO

- Estimar modelos ARIMA y realizar predicciones

#### DESARROLLO
Utilizando el siguiente vector numérico, realiza lo que se indica:
```R
url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")
```
- [global.txt](/global.txt)

1) Crear una objeto de serie de tiempo con los datos de Global. La serie debe ser mensual 
comenzado en Enero de 1856

"En este apartado se construirá la serie de tiempo con los datos de Global.
La serie comenzó en enero de 1856 y su periodicidad fue de tipo mensual:"

```r
Global.ts <- ts(Global, start = c(1856,1), freq = 12)
Global.ts
class(Global.ts)
##[1] "ts"
```


2) Realizar una gráfica de la serie de tiempo anterior de 2005

"A continuación se realizará el gráfico de la serie de tiempo elaborada en el apartado
anterior:"
```r
plot(Global.ts, 
     main = "Serie de tiempo de los datos de Global", 
     xlab = "Tiempo",
     ylab = "Temperatura",
     sub = "Mensual de Enero de 1856 - Diciembre de 2005",
     col = "blue")
grid()
```
"Con base en los resultados obtenidos, puede concluirse que la serie no es estacionaria 
debido a que ésta no muestra un comportamiento estable a lo largo del tiempo, es decir, 
su media y varianza no son constantes en el tiempo."

3) Realizar una gráfica de la serie de tiempo anterior, transformando a la 
primera diferencia.

```r
plot(diff(Global.ts), xlab = "", ylab = "",col = "blue")
title(main = "Serie Diferenciada de los datos de Global",
      xlab = "Tiempo", ylab = "Dif Serie",
      sub = "Gráfica de la serie diferenciada de Primer Órden",
      )
grid()
```

4) ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?

"Con base en los resultados obtenidos, puede concluirse que la serie es estacionaria en primera
diferencia debido a que ésta muestra un comportamiento estable a lo largo del tiempo, es decir, 
su media y varianza son constantes en el tiempo."

5) Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial:

```r
acf(diff(Global.ts))
pacf(diff(Global.ts))
```

"6. De acuerdo con el gráfico de la función de autocorrelación puede estimarse que el
modelo ARIMA a construir deberá contener entre 1 y 3 términos autorregresivos. Por
otro lado, la gráfica de la función de autocorrelación parcial sugiere que el modelo
ARIMA es de orden 1 para la media móvil. Finalmente, el modelo ARIMA que se construirá
tendrá un orden de integración-diferenciación de uno."

"Por todo lo anterior, se construirán tres modelos ARIMA, y se seleccionará el mejor
con base en el criterio del AIC:"
```r
arima1 <- arima(Global.ts,order = c(1,1,1))
arima1
# Call:
#   arima(x = Global.ts, order = c(1, 1, 1))
# 
# Coefficients:
#         ar1      ma1
#       0.3797  -0.8700
# s.e.  0.0433   0.0293
# 
# sigma^2 estimated as 0.01644:  log likelihood = 1142.13,  aic = -2278.26

arima2 <- arima(Global.ts,order = c(2,1,1))
arima2
# Call:
#   arima(x = Global.ts, order = c(2, 1, 1))
# 
# Coefficients:
#         ar1     ar2      ma1
#       0.4406  0.1417  -0.9608
# s.e.  0.0262  0.0256   0.0107
# 
# sigma^2 estimated as 0.01622:  log likelihood = 1153.79,  aic = -2299.59

arima3 <- arima(Global.ts,order = c(3,1,1))
arima3
# Call:
#   arima(x = Global.ts, order = c(3, 1, 1))
# 
# Coefficients:
#   ar1     ar2     ar3      ma1
# 0.4403  0.1260  0.0492  -0.9669
# s.e.  0.0251  0.0262  0.0247   0.0085
# 
# sigma^2 estimated as 0.01619:  log likelihood = 1155.77,  aic = -2301.53
```

"7. Con base en los resultados anteriores, puede concluirse que el modelo ARIMA 3
tiene el menor AIC (-2301.53). En seguida, se realizará el ajuste del modelo:"
```r
fit <- arima(Global.ts, order = c(3, 1, 1))
fit
# Call:
#   arima(x = Global.ts, order = c(3, 1, 1))
# 
# Coefficients:
#         ar1     ar2     ar3      ma1
#       0.4403  0.1260  0.0492  -0.9669
# s.e.  0.0251  0.0262  0.0247   0.0085
# 
# sigma^2 estimated as 0.01619:  log likelihood = 1155.77,  aic = -2301.53
```

#"La ecuación del modelo es la siguiente:

"Δx^_t = 0.4403*Δx_t-1 + 0.1260*Δx_t-2 + 0.0492*Δx_t-3 - 0.9669*u_t-1"

"8. Finalmente se realizarán los pronósticos para el año 2006 y su respectivo gráfico:"
```r
pr <- predict(fit,12)$pred
pr
#          Jan       Feb       Mar       Apr       May       Jun       Jul
# 2006 0.3928808 0.4068537 0.4148283 0.4244244 0.4303420 0.4345490 0.4376192
#          Aug       Sep       Oct       Nov       Dec
# 2006 0.4397923 0.4413430 0.4424507 0.4432407 0.4438044
##
ts.plot(cbind(window(Global.ts, start = 2000), pr), col = c("blue", "red"), xlab = "")
title(main = "Pronósticos para el año 2006 de la serie de los datos de Global",
      xlab = "Mes",
      ylab = "Mediciones de Global")
```

#### Ir al archivo de código fuente
- [PostWork 7](https://github.com/alsolisc/Postworks/tree/main/src/PostWork7.R)
