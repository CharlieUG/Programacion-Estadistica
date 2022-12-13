## Postwork 7. Predicciones de la temperatura global

### OBJETIVO

- Estimar modelos ARIMA y realizar predicciones

#### DESARROLLO
Utilizando el siguiente vector numérico, realiza lo que se indica:
```R
url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")
```

1) Crear una objeto de serie de tiempo con los datos de Global. La serie debe ser mensual 
comenzado en Enero de 1856

2) Realizar una gráfica de la serie de tiempo anterior de 2005

3) Realizar una gráfica de la serie de tiempo anterior, transformando a la 
primera diferencia.

4) ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?

5) Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial?

6) De acuerdo con lo observado en las gráficas anteriores, se sugiere un modelo ARIMA
con AR(1), I(1) y MA desde 1 a 4 rezagos Estima los diferentes modelos ARIMA propuestos.

7) Con base en el criterio de Akaike, estima el mejor modelo ARIMA y realiza una 
predicción de 12 periodos (meses).


#### Ir al archivo de código fuente
- [PostWork 7](src/PostWork7.R)
