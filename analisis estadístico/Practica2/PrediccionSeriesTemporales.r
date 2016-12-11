## -------------------------------------------------------------------------
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias y establecimiento de directorio #####

if (!require("forecast")){
  install.packages("forecast") 
  library(forecast)
}
##Establecemos el path de trabajo
setwd("C:\\Desarrollo\\repos\\Master-BigData-CIFF-2016\\analisis estadístico\\Practica2")

##### 2. Bloque de Modelos Estacionarios #####
data = read.csv('monthly-milk-production-pounds-p.csv',header = TRUE, skip=0, sep = ";")
class(data)
data$Year <- format(as.Date(paste0(as.character(data[,1]),"-01"),format = "%Y-%m-%d"),"%Y")
data$Month <- format(as.Date(paste0(as.character(data[,1]),"-01"),format = "%Y-%m-%d"),"%b")
colnames(data) <- c("month","milk","year")
data <- na.omit(data)
head(data)
min(data$year)
max(data$year)
milk_ts=ts(data$milk,start=1962,frequency=12)
milk_ts
## -------------------------------------------------------------------------
cycle(milk_ts)
#Ploteamos y vemos que es una serie creciente en el tiempo, agregando las medias 
plot(aggregate(milk_ts,FUN = mean))
# vemos que tiene cierta estacionalidad en los meses de Mayo y Junio
#donde son mayores las producciones de leche
boxplot(milk_ts~cycle(milk_ts))

#Ploteamos la serie y vemos que tiene tendencia, lo que  indica que no
#es estacionaria y da pistas de que tendremos que hacel al menos una diferenciacion 
#para eliminar la tendencia
plot(milk_ts)
abline(reg=lm(milk_ts~time(milk_ts)))
#adf.test(diff(log(milk_ts)), alternative="stationary", k=0)

#Ploteamos 4 años la serie normal
acf(milk_ts,lag.max=48)
pacf(milk_ts,lag.max=48)
tsdisplay(milk_ts)

#calculamos las primeras diferencias y ploteamos
milk_ts_d1=diff(milk_ts,lag=1,differences=1)
plot(milk_ts_d1)
acf(milk_ts_d1,lag.max=48)
pacf(milk_ts_d1,lag.max=48)
tsdisplay(milk_ts_d1)
#Vemos que en la primera diferencia,tanto las correlaciones como las correlaciones
#parciales están fuera del marge, por lo que siguen siendo no estacionarias


#Calculamos las segundas diferencias, y vemos que las autocorrelaciones tienden
#a cero despues de el primer año y que las correlaciones parciales, todavía
#no
milk_ts_d1_d12=diff(diff(milk_ts, lag=1,differences=1),lag=12,differences=1)
plot(milk_ts_d1_d12)
acf(milk_ts_d1_d12,lag.max=48)
pacf(milk_ts_d1_d12,lag.max=48)
tsdisplay(milk_ts_d1_d12)

#Aplicamos el autoarima para ver que nos indica
Auto_milk_model=auto.arima(milk_ts,trace=TRUE)
plot(forecast(Auto_milk_model,h=24))
f1 = forecast(Auto_milk_model,24)
f1
?forecast
summary(Auto_milk_model)
tsdisplay(residuals(Auto_milk_model))
#Parece que la serie tiene media movil, y nos da que el mejor modelo es aquel
#que se realiza con una sola diferencia. la gráficas muestran como es estacionaria
