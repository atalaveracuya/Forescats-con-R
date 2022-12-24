
##### Capítulo 8 - Modelos ARIMA ######
#### Book: https://otexts.com/fpp2
### configuraciones iniciales ####

rm(list=ls())


# Definicion del directorio de trabajo

setwd("D:/ANDRES/Documentos/Seriesdetiempo/")

# Verificacion del directorio de trabajo
getwd()

#install.packages("seasonal")
#install.packages("fpp2")

library(seasonal)
library(fpp2)
#library(fpp)

Box.test(diff(goog200), lag=10, type="Ljung-Box")
#> 
#>  Box-Ljung test
#> 
#> data:  diff(goog200)
#> X-squared = 11, df = 10, p-value = 0.4
#> 

#### Ejemplo 1 #####

#### Data original, logaritmo de a10 y diferencia estacional de rezago 12 ####
cbind("Sales ($million)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")

# Logaritmos y diferencias estacionales de los datos de ventas de A10 (antidiabéticos). 
# IMPORTANTE: 
# Los logaritmos estabilizan la varianza, mientras que la diferenciación ayuda a  
# eliminar la estacionalidad y la tendencia.



### Ejemplo 2 ####

help(usmelec)
cbind("Billion kWh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" =
        diff(log(usmelec),12),
      "Doubly\n differenced logs" =
        diff(diff(log(usmelec),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly US net electricity generation")

##Recomendación:
# si los datos tienen un fuerte patrón estacional, recomendamos que primero 
# se realice la diferenciación estacional, 
# porque la serie resultante a veces será estacionaria y no habrá necesidad 
# de una primera diferencia adicional.

# Interpretación:
# "Las primeras diferencias" son el cambio entre una observación y la siguiente. 
# "Las diferencias estacionales" son el cambio entre un año y el siguiente.


## ¿Cuando hacer diferenciación? 
## Una herramienta es la Prueba de raíz unitaria

# Prueba de Kwiatkowski-Phillips-Schmidt-Shin (KPSS) 
#Ho: La serie es estacionaria 
#H1: La serie no es estacionaria 
#Sí el estadístico de prueba > p-value , se rechaza la Ho al (1-p-value)x100 
# Si se rechaza que la serie es estacionaria, entonces se requiere una diferenciación.

autoplot(goog)
library(urca)
goog %>% ur.kpss() %>% summary()
#> 
#> ####################### 
#> # KPSS Unit Root Test # 
#> ####################### 
#> 
#> Test is of type: mu with 7 lags. 
#> 
#> Value of test-statistic is: 10.72 
#> 
#> Critical value for a significance level of: 
#>                 10pct  5pct 2.5pct  1pct
#> critical values 0.347 0.463  0.574 0.739
#> 

goog %>% diff() %>% ur.kpss() %>% summary()

#0.0324>0.01
#Se acepta la Ho al 95%. La serie es estacionaria al 95% 
#Se rechaza la Ho al 99%. La serie es no es estacionaria al 99% 

#Si primero se hace la primera diferenciación

#Secuencia de pruebas KPSS. 
#Sirve para calcular el número apropiado de primeras diferencias

ndiffs(goog)
#> [1] 1

autoplot(usmelec)
usmelec %>% log() %>% nsdiffs()
#> [1] 1
usmelec %>% log() %>% diff(lag=12) %>% ndiffs()
#> [1] 1
#> 
usmelec %>% log() %>% diff(lag=12) %>% diff(lag=1) %>% ndiffs()



# Test de Dickey Fuller aumentado (prueba de raíz unitaria)
library(tseries)
adf.test(diff(goog))
#Rechaza la hipótesis nula. Por lo tanto los datos de la diferencia de goog 
# son estacionarios


# Modelos autorregresivos 
# AR(p) model 
# 

### Modelos de promedio móvil
# MA(q) model 
# Un modelo de promedio móvil se usa para pronosticar valores futuros, mientras
# que el suavizado de promedio móvil se usa para estimar el ciclo de tendencia 
# de valores pasados

#the process is invertible when |??|< 1

### Modelos ARIMA no estacionales

# ARIMA(q,d,p)
# q: orden del modelo autorregresivo  
# d: grado de diferenciación 
# p: orden del modelo de promedio móvil 

# Las mismas condiciones de estacionariedad e invertibilidad que se utilizan
# para los modelos autorregresivos y de promedio móvil también se aplican a un
# modelo ARIMA.

# Ruido blanco (White noise)	  ARIMA(0,0,0)
# Paseo aleatorio	(Random walk) ARIMA(0,1,0) sin constante
# Paseo aleatorio con deriva    ARIMA(0,1,0) con una constante
# (Random walk with drift)        
# Autoregression	              ARIMA(p,0,0)
# Media móvil	                  ARIMA(0,0,q)
# (Moving average)

# Ejemplo 1 
# Gasto de consumo de EE. UU.

autoplot(uschange[,"Consumption"]) +
  xlab("Year") + ylab("Quarterly percentage change")

#Paso 1: Ver la serie original si tiene estacionalidad.
# Como no tiene estacionalidad, entonces usaremos un modelo ARIMA no estacional 

#Paso 2: Seleccionamos un modelo automáticamente 
(fit <- auto.arima(uschange[,"Consumption"],seasonal=FALSE))
#Pronóstico 
fit %>% forecast(h=10) %>% autoplot(include=80)

#Paso 3: Revisar el pronóstico, se debe cumplir lo siguiente según sea el caso

#Para el valor promedio futuro

#Si C=0 y d=0, los pronósticos a largo plazo se reducirán a cero.
#Si C=0 y d=1, los pronósticos a largo plazo irán a una constante distinta de cero.
#Si C=0 y d=2, las previsiones a largo plazo seguirán una línea recta.
#Si C???0 y d=0, las previsiones a largo plazo irán a la media de los datos.
#Si C???0 y d=1, las previsiones a largo plazo seguirán una línea recta.
#Si C???0 y d=2, los pronósticos a largo plazo seguirán una tendencia cuadrática.

#Para el intervalo de confianza del promedio futuro
#cuanto mayor sea el valor de d, más rápidamente aumentan de tamaño los 
#intervalos de predicción. Si d=0 los intervalos no cambian mucho. 

#Si la serie original tuviera ciclo: 
# El valor de "p" es importante si los datos muestran ciclos. 
# Para obtener pronósticos cíclicos, es necesario tener p???2, junto con algunas 
# condiciones adicionales sobre los parámetros.

#¿Cuánto dura en promedio un ciclo? 
#https://otexts.com/fpp2/non-seasonal-arima.html#fn15
fi1 <-0.5
fi2 <-0.5
duracion_ciclo <- (2*pi)/acos(-fi1*(1-fi2)/(4*fi2))
print(duracion_ciclo)

##Comentario de la gráfica: 
# Este comportamiento se ve en la Figura donde d=0 y C???0
# En esta figura, los intervalos de predicción son casi los mismos para los
# últimos horizontes de pronóstico y los pronósticos puntuales son iguales a 
# la media de los datos.

#### Paso 4: Gráfica de autocorrelación ACF y autocorrelación parcial PACF  ####
# Mide la relación entre yt-yt-k después de eliminar los efectos de los retrasos
# de 1,2,3,4,....,k-1. 

#Figura 8.9: ACF de cambio porcentual trimestral en el consumo de EE. UU
ggAcf(uschange[,"Consumption"])
#Figura 8.10: PACF de cambio porcentual trimestral en el consumo de EE. UU.
ggPacf(uschange[,"Consumption"])

### Si ARIMA(p,d,0) o ARIMA(0,d,q) las gráficas ACF y PACF pueden ser útiles 
# para determinar el valor de p o q.

## Si p y q son ambos positivos entonces las gráficas no ayudan a encontrar los 
# valores adecuados de p y q

## Cuando ARIMA(p,d,0) ocurre lo siguiente:
# - El ACF es exponencialmente decreciente o sinuoidal 
# - Hay un pico significativo del retrazo p en el PACF, pero ninguno más allá
# del retrazo p. 

## Cuando ARIMA(0,d,q) ocurre lo siguiente:
# - el PACF es exponencialmente decreciente o sinusoidal
# - hay un pico significativo en el retraso q en el ACF, pero ninguno más allá
# del retraso q .


### conclusión del paso 4: 
#Observando las gráficas ACF y PACF el modelo para pronosticar es el ARIMA(3,0,0)

#¿Cuál es el mejor? 

# ARIMA(1,0,3) o ARIMA(3,0,0)
#Usamos el criterio de información de Akaike corregido (AICc)
(fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0)))
#Se usa "seasonal=FALSE" para evitar que busque modelos ARIMA estacionales

# AIC: criterio de información de Akaike
# BIC: criterio de información Bayesiano

##IMPORTANTE: Sólo sirven para seleccionar el p y q, más no la diferenciación d. 
# Por eso, primero seleccionamos el d con algún método y luego los otros.  


####### EJEMPLO FINAL ARIMA #########

# Ejemplo: Pedidos de equipos eléctricos ajustados estacionalmente
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

eeadj %>% diff() %>% ggtsdisplay(main="")

(fit0 <- Arima(eeadj, order=c(3,1,0)))
(fit1 <- Arima(eeadj, order=c(4,1,0)))
(fit2 <- Arima(eeadj, order=c(2,1,0)))
(fit3 <- Arima(eeadj, order=c(3,1,1)))

#El mejor modelo es el ARIMA(3,1,1)
checkresiduals(fit3)

autoplot(forecast(fit))

#Con el autoarima

# stepwise=FALSE y approximation=FALSE para buscar todos los modelo posibles.  
# seasonal=FALSE para evitar que busque modelos ARIMA estacionales;
 
(fitx <- auto.arima(eeadj,stepwise=FALSE,
                    approximation=FALSE,
                    seasonal=FALSE))