##### Capítulo7 - Descomposición de serie temporal ######
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

oildata <- window(oil, start=1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")

#Weighted average form
#Component form
#Flat forecasts 
#Recuerde que estos pronósticos solo serán adecuados 
#si la serie temporal no tiene tendencia o componente estacional.

#lt el nivel (o el valor suavizado) de la serie en el tiempo. 

#In particular, for simple exponential smoothing, we need to select the values of  
#?? and ???

#### Método para estimar el ?? and ??? ###
# Hay método subjetivo y objetivo 

#### Métodos objetivos ##### 
#### Suavizado exponencial simple ####


oildata <- window(oil, start=1996)
# Estimate parameters
fc <- ses(oildata, h=5)
# Precisión de los errores de entrenamiento de un paso adelante
round(accuracy(fc),2)
#>               ME  RMSE   MAE MPE MAPE MASE  ACF1
#> Training set 6.4 28.12 22.26 1.1 4.61 0.93 -0.03

oildata <- window(oil, start=1996)
# Estimate parameters
fc <- ses(oildata, h=5)
summary(fc[["model"]])

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")

#La serie estimada se suaviza más con un valor ?? más pequeño. 

#### Método de tendencias de Holt #### 

air <- window(ausair, start=1990)
fc <- holt(air, h=5)
summary(fc[["model"]])


window(ausair, start=1990, end=2016) %>%
  holt(h=5, PI=FALSE) %>%
  autoplot()

#El valor muy pequeño de b* significa que 
#la pendiente apenas cambia con el tiempo.

#### Método de tendencia amortiguada #####
fc <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))


dat <- read.csv(
  file = "serie_empleo.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactors = TRUE
)

#Crear un objeto de la clase ts: serie de tiempo.
x <- ts(dat[,1], frequency = 12, start = c(2015, 1))

fc <- holt(x, h=15)
fc2 <- holt(x, damped=TRUE, phi = 0.9, h=15)
autoplot(x) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method of formal employment in Perú ") + xlab("Year") +
  ylab("Miles de puestos de trabajo (miles)") +
  guides(colour=guide_legend(title="Forecast"))

# Ovejas en Asia 
autoplot(livestock) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)
# Compare MSE (Error cuadrático medio):
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
# Compare MAE (Error medio absoluto):
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)

fc <- holt(livestock, damped=TRUE)
# Estimated parameters:
fc[["model"]]
autoplot(fc) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")



livestock2 <- window(livestock, start=1970,
                     end=2000)
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2, damped = TRUE)

accuracy(fit1, livestock)
accuracy(fit2, livestock)
accuracy(fit3, livestock)

#Lo que requerimos de un método de pronóstico son pronósticos 
#consistentemente sensibles, y estos deben evaluarse con frecuencia 
#contra la tarea en cuestión.

# formal employment in Perú

autoplot(x) +
  xlab("Year") + ylab("Miles de puestos de trabajo (miles)")

e1 <- tsCV(x, ses, h=6)
e2 <- tsCV(x, holt, h=6)
e3 <- tsCV(x, holt, damped=TRUE, h=6)
# Compare MSE (Error cuadrático medio):
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
# Compare MAE (Error medio absoluto):
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)

fc <- ses(x,h=6)
# Estimated parameters:
fc[["model"]]
autoplot(fc) +
  xlab("Year") + ylab("Miles de puestos de trabajo (miles)")

#### Método de Holt-Winters con estacionalidad #####

# Example: International tourist visitor nights in Australia

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

# prefiere el método aditivo cuando las variaciones estacionales 
# son aproximadamente constantes a lo largo de la serie, mientras 
# que se prefiere el método multiplicativo cuando las variaciones 
# estacionales cambian proporcionalmente al nivel de la serie. 


#### Modelos de estado espacio para el suavizado exponencial 

# ETS(A,N,N): suavizado exponencial simple con errores aditivos
# ETS(M,N,N): suavizado exponencial simple con errores multiplicativos
# ETS(A,A,N): método lineal de Holt con errores aditivos
# ETS(M,A,N): método lineal de Holt con errores multiplicativos
# Otros modelos ETS

#### Estimación y selección del modelo ####

# ets(y, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
#     gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
#     additive.only=FALSE, restrict=TRUE,
#     allow.multiplicative.trend=FALSE)
help(ets)

help(bicoal)