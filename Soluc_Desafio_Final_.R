
#### Desarrollo de Trabajo Final #######


### configuraciones iniciales ####

rm(list=ls())


# Definicion del directorio de trabajo

setwd("D:/ANDRES/Documentos/Seriesdetiempo/")

# Verificacion del directorio de trabajo
getwd()


# Cargar la librer?a####
library(fpp2)
library(seasonal)
library("ggplot2")
library(urca)
library(tseries)
library(dplyr)
library(xlsx)


####

# 1. Considere la serie pigs: La cantidad de cerdos sacrificados cada mes.
# a. Use la funci?n ses() para encontrar los valores ?ptimos de ?? y ???. Y genere pron?sticos
# para los pr?ximos 4 meses.

help(pigs)

pigsdata <- window(pigs, start=1980)

autoplot(pigsdata) +
  ggtitle("Total de cerdos sacrificados en Victoria-Australia") + 
  ylab("Total") + xlab("Year")

# Estimate parameters
fc <- ses(pigsdata, h=4)
# Precisi?n de los errores de entrenamiento de un paso adelante
round(accuracy(fc),2)
summary(fc[["model"]])

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Total") + xlab("Year")


autoplot(fc) +
  autolayer(fitted(fc), series="SES method") +
  ggtitle("Forecasts SES de cerdos sacrificados en Victoria-Australia") + xlab("Year") +
  ylab("Total") +
  guides(colour=guide_legend(title="Forecast"))


#### Ejercicio 2 ####

books
## 2.1 
help(books)
autoplot(books) +
  ggtitle("Ventas diarias de libros de bolsillo y tapa dura") + 
  ylab("Total") + xlab("D?as")

## 2.2 

bookbols <- ts(books[,1], start =1)
bookduro <- ts(books[,2], start =1)

# Estimar par?metros books de bolsillo
fc1 <- ses(bookbols, h=4)
round(accuracy(fc1),2)

# Estimar par?metros books de tapa dura 
fc2 <- ses(bookduro, h=4)
round(accuracy(fc2),2)

autoplot(fc1) +
  autolayer(fitted(fc1), series="Serie suavizada", PI=TRUE) + 
  autolayer(fc1, series="pron?stico") + 
  ggtitle("Forecasts SES de ventas diarias de libros de bolsillo ") + xlab("Year") +
  ylab("") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(fc2) +
  autolayer(fitted(fc2), series="Serie suavizada", PI=TRUE) + 
  autolayer(fc2, series="pron?stico") + 
  ggtitle("Forecasts SES de ventas diarias de libros de tapa dura ") + xlab("Year") +
  ylab("") +
  guides(colour=guide_legend(title="Forecast"))

par(mfrow = c(1,2))
plot(fc1, main="Forecasts SES libros de bolsillo")
plot(fc2, main="Forecasts SES libros de tapa dura")


# 2.3 

# Estimar par?metros books de bolsillo
fc1 <- ses(bookbols, h=4)
round(accuracy(fc1),2)

# Estimar par?metros books de tapa dura 
fc2 <- ses(bookduro, h=4)
round(accuracy(fc2),2)


#### Ejercicio 3 ####

# a. Aplicar el m?todo lineal de Holt a las series paperback y las hardback. 
# Calcular los pron?sticos de cuatro d?as en cada caso.


#### M?todo de tendencias de Holt #### 

# books de bolsillo y tapa dura

fc1 <- holt(bookbols, h=4)
fc2 <- holt(bookduro, h=4)

autoplot(fc1) +
  autolayer(fitted(fc1), series="Serie suavizada", PI=TRUE) + 
  autolayer(fc1, series="pron?stico") + 
  ggtitle("Forecast lineal Holt de ventas diarias de libros de bolsillo ") + xlab("Year") +
  ylab("") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(fc2) +
  autolayer(fitted(fc2), series="Serie suavizada", PI=TRUE) + 
  autolayer(fc2, series="pron?stico") + 
  ggtitle("Forecast lineal de Holt de ventas diarias de libros de tapa dura ") + xlab("Year") +
  ylab("") +
  guides(colour=guide_legend(title="Forecast"))

par(mfrow = c(1,2))
plot(fc1, main="libros de bolsillo")
plot(fc2, main="libros de tapa dura")

#3.b
# libro de bolsillo 
fcses <- ses(bookbols, h=4)
fcholt <- holt(bookbols, h=4)
round(accuracy(fcses),2)
round(accuracy(fcholt),2)

# libros de tapa dura 
fcses2 <- ses(bookduro, h=4)
fcholt2 <- holt(bookduro, h=4)
round(accuracy(fcses2),2)
round(accuracy(fcholt2),2)


#3.c
autoplot(bookbols) +
  autolayer(fitted(fcses), series="suavizado SES") + 
  autolayer(fitted(fcholt), series="suavizado Holt") + 
  autolayer(fcses, series="pron?stico SES", PI=FALSE) + 
  autolayer(fcholt, series="pron?stico lineal Holt", PI=FALSE) +
  ggtitle("Forecast  SES y  lineal Holt de ventas diarias de libros de bolsillo ") + 
  xlab("D?as") +
  ylab("") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(bookduro) +
  autolayer(fitted(fcses2), series="suavizado SES") + 
  autolayer(fitted(fcholt2), series="suavizado Holt") + 
  autolayer(fcses2, series="pron?stico SES", PI=FALSE) + 
  autolayer(fcholt2, series="pron?stico lineal Holt", PI=FALSE) +
  ggtitle("Forecast  SES y  lineal Holt de ventas diarias de libros de tapa dura ") + 
  xlab("D?as") +
  ylab("") +
  guides(colour=guide_legend(title="Forecast"))

#3.d 


#### Ejercicio 4 ####

#4a. ?Por qu? es necesaria la estacionalidad multiplicativa para esta serie?

retaildata <- readxl::read_excel("retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))
autoplot(myts) 
ggseasonplot(myts)
ggsubseriesplot(myts)

# 4.b
fit1 <- hw(myts,seasonal="multiplicative")

fit2 <- hw(subset(myts,end=length(myts)),
           damped = TRUE, seasonal="multiplicative")
autoplot(myts) +
  autolayer(fit1, series="HW multiplicativo ",
            PI=FALSE) +
  autolayer(fit2, series="HW multiplicativo amortiguado ",
            PI=FALSE) +
  xlab("year") +
  ylab("ventas") +
  ggtitle("ventas minoristas australiana") +
  guides(colour=guide_legend(title="Forecast"))

# 4.c 
round(accuracy(fit1),2)
round(accuracy(fit2),2)

# 4.d 
checkresiduals(fit1)

#### Ejercicio 5 ####

## 5.a 
help(bicoal)
autoplot(bicoal) +
  ggtitle("Producci?n anual de bituminous coal 1920 - 1968") + 
  ylab("") + xlab("A?o")

## 5.b
arima.bicoal <- Arima( bicoal, order=c(4,0,0) )
arima.bicoal

## 5.c
help(bicoal)
ggAcf(bicoal)
ggPacf(bicoal)

(arima1.bicoal <- Arima(bicoal, order=c(4,0,0)))
ndiffs(bicoal)
(arima2.bicoal <- Arima(bicoal, order=c(3,0,0)))

#Al observar las gr?ficas, proponemos dos modelos y elegimos al que tiene 
#un menor valor del criterio de informaci?n Akaike corregido ACCc

#Modelo 1: ARIMA(4,0,0)
#Modelo 2: ARIMA(3,0,0)

#### Ejercicio 6 ####

o3_data <- read.csv(
  file = "processed_o3_data.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactors = TRUE
)
tail(o3_data) 


# Detectar y contar los valores faltantes 
which(is.na(o3_data$O3_BA))
sum(is.na(o3_data$O3_BA))
#Hay 194 missing en la serie que queremos analizar 

#?Reemplazar con ceros?
#o3_data[is.na(o3_data)] <- 0
#o3_data


#?Reemplazar con el promedio?
o3_data$O3_BA[is.na(o3_data$O3_BA)] <- mean(o3_data$O3_BA, na.rm = TRUE)
#o3_data$O3_BA <- round(o3_data$O3_BA, digits = 0)

#Crear un objeto de la clase ts: serie de tiempo.
help(ts)
o3a <- ts(o3_data[,1], frequency = 7, start = c(2018-01-01))

#Analizar la data ajustada estacionalmente 
o3a %>% stl(s.window='periodic') %>% seasadj() -> o3aadj
autoplot(o3aadj)

# Paso 1 : la gr?fica inicial 
autoplot(o3aadj) +
  ylab("Ozono BA") + xlab("D?as")

# Paso 2: Transformaci?n Box Cox para estabilizar la varianza 
(lambda <- BoxCox.lambda(o3aadj))
autoplot(BoxCox(o3aadj,lambda))
#No utilizamos la transformaci?n Box-Cox porque no simplifica los patrones de 
#los datos hist?ricos (tendencia, estacional y ciclo).  

#Paso 3
#?cu?ntas diferencias requiere la serie original?
ndiffs(o3aadj)
#S?lo una diferenciaci?n 
o3aadj %>% diff() %>% ggtsdisplay(main="")
# Con la primera diferencia la serie se vuelve estacionaria

#Paso 4: Observar el ACF y PACF para proponer modelos ARIMA
# Al observar el ACF y PACF se propone el ARIMA(1,1,0), ARIMA(6,1,0) 

#Por otro lado, el camino corto propone el ARIMA(2,1,2)
(fit_o3a <- auto.arima(o3aadj,stepwise=FALSE,
                       approximation=FALSE,
                       seasonal=FALSE))
# Paso 5: Ajustamos los modelos propuestos y elegimos el que tiene el menor valor
# AICc

(fit1 <- Arima(o3aadj, order=c(1,1,0)))
(fit2 <- Arima(o3aadj, order=c(6,1,0)))
(fit3 <- Arima(o3aadj, order=c(2,1,2)))


 # elegimos el ARIMA(2,1,2) para hacer el pron?stico

# Paso 6: Test de ruido blanco para los residuos del mejor modelo

checkresiduals(fit3)


# Los rezagos de los resideuos est?n denttro de las bandas, lo que sugiere que 
# es ruido blanco 

#Paso 7: Proposticar el modelo elegido

autoplot(forecast(fit3, h=90))


#### ejercicio 7 #####

##Paso 0: Carga de la data y limpieza  

df_cancer <- read.csv(
  file = "Cases.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactors = TRUE
) 

# Detectar y contar los valores faltantes 
which(is.na(df_cancer$Freq))
sum(is.na(df_cancer$Freq))

#Crear un objeto de la clase ts: serie de tiempo.
help(ts)
cancer0 <- ts(df_cancer[,3], frequency = 1, start = 1958)


#Observamos la data del 2018 muy por debajo de la realidad. Lo quitamos del an?lisis 
df2_cancer <- subset(df_cancer,Year != 2018 )
cancer <- ts(df2_cancer[,3], frequency = 1, start = 1958)

par(mfrow = c(1,2))
plot(cancer0, main="Serie de c?ncer en Lima")
plot(cancer, main="Serie de c?ncer en Lima, sin el 2018")


#b. Dividimos el conjunto de datos en un conjunto de entrenamiento
#y un conjunto de prueba, donde el conjunto de prueba son los 
#?ltimos 12 a?os de datos.

cancer_train <- window(cancer,start=c(1958),end=c(2005))
cancer_test <- window(cancer,start=c(1958))

# Paso 2: Transformaci?n Box Cox para estabilizar la varianza 
(lambda <- BoxCox.lambda(cancer_train))
cancer_boxcox <- BoxCox(cancer_train,lambda)
autoplot(cancer_boxcox)
#lambda = 1.259332
par(mfrow = c(1,2))
plot(cancer_train, main="Serie original de c?ncer")
plot(cancer_boxcox, main="Serie Box-Cox de c?ncer")


#No utilizamos la transformaci?n Box-Cox porque no simplifica los patrones de 
#los datos hist?ricos y no estabiliza la varianza.   

#Paso 3
#?cu?ntas diferencias requiere la serie transformada de c?ncer?
cancer_train %>% ur.kpss() %>% summary()
cancer_train %>% diff() %>% ur.kpss() %>% summary()
cancer_train %>% diff() %>% diff() %>% ur.kpss() %>% summary()
#Segun esta prueba se debe hacer dos diferencias para que la serie sea estacionaria


#Secuencia de KPSS
ndiffs(cancer_train)
#Seg?n la secuencia del test KPSS se requiere una diferencia para que la serie
#transformada box cox de c?ncer se vuelva estacionaria. 


# Test de Dickey Fuller aumentado (prueba de ra?z unitaria)
adf.test(cancer_train)
adf.test(diff(cancer_train))
adf.test(diff(diff(cancer_train)))
#Seg?n el test Dickey Fuller se requiere dos diferencias para que la serie
#transformada se vuelva estacionaria. 

#Visualmente 
cancer_train %>% diff() %>% ggtsdisplay(main="")
# Al observar la data transformada de cancer s?lo se necesita una diferencia 
# para que la serie se vuelva estacionaria tal como nos sugiere la funci?n  
# "ndiffs". 

#Paso 4: Observar el ACF y PACF para proponer modelos ARIMA. Adem?s usar el auto.arima 
# Al observar el ACF y PACF se propone el ARIMA(1,1,0) ARIMA(1,1,1)

#Por otro lado, el camino corto auto.arima propone el ARIMA(1,1,0)
(fit_cancer <- auto.arima(cancer_train, 
                          stepwise=FALSE,
                          approximation=FALSE,
                          seasonal=FALSE))

# Paso 5: Ajustamos los modelos propuestos y elegimos el que tiene el menor valor
# AICc

(fit1 <- Arima(cancer_train , order=c(1,1,1)))
(fit2 <- Arima(cancer_train , order=c(1,1,0)))

# elegimos el ARIMA(1,1,0) para hacer el pron?stico

# Paso 6: Test de ruido blanco para los residuos del mejor modelo

cancer_fit <- forecast(fit2,h=17)
checkresiduals(cancer_fit)

checkresiduals(fit2)

#Paso 7: Pronosticar el modelo elegido

autoplot(cancer_fit) +
  autolayer(cancer_fit, series = "Pron?stico ARIMA(1,1,0)") + 
  autolayer(cancer_test, series = "casos de c?ncer") + 
xlab("A?os") + ylab("casos de c?ncer") +
  ggtitle("Forecasts para los casos de c?ncer en Lima") +
  guides(colour=guide_legend(title="Leyenda"))



#### ejercicio 8 #####

###como la 4 #######


##Paso 0: Carga de la data y limpieza  

df_mercado <- read.csv(
  file = "MERCADO_REGULADO.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = TRUE
) 

# Detectar y contar los valores faltantes 
which(is.na(df_mercado$ENERGIA))
sum(is.na(df_mercado$ENERGIA))


#### M?todo de Holt-Winters conn datos diarios ######
mercado0 <- ts(df_mercado[,3], frequency = 7 , start =c(2019,60))
autoplot(mercado0)

fc <- hw(subset(mercado0,end=length(mercado0)-35),
         damped = TRUE, seasonal="multiplicative", h=50)

autoplot(fc, PI=FALSE) +
  autolayer(fc, series = "HW multi damped", PI=FALSE) + 
  autolayer(mercado0, series = "consumo de energ?a KW/h") + 
  xlab("A?os") + ylab("KW/h") +
  ggtitle("Forecast diario para la demanda de energ?a en KW/h") +
  guides(colour=guide_legend(title="Leyenda"))


write.xlsx(fc, "fc.xlsx")
