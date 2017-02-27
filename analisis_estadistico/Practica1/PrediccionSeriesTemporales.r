## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias y establecimiento de directorio #####

if (!require("forecast")){
  install.packages("forecast") 
  library(forecast)
}

setwd("D:/Documentos, Trabajos y Demás/Formación/Master CIFF/2016 2017 Análisis Estad�?stico/Executive Sesión 2")

## -------------------------------------------------------------------------
##       PARTE 1: SERIES TEMPORALES ARIMA
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 2. Bloque de Modelos Estacionarios #####

Estacionaria=rnorm(200,mean=10,sd=3)
plot(Estacionaria,type="l")

acf(Estacionaria)
pacf(Estacionaria)

RuidoBlanco=rnorm(200,mean=0,sd=3)
plot(RuidoBlanco,type="l")

acf(RuidoBlanco)
pacf(RuidoBlanco)

## -------------------------------------------------------------------------

##### 3. Bloque de Modelos Autoregresivos AR #####

set.seed(12345)
ruido=rnorm(200,mean=0,sd=2)
AR1=c(5)
alpha=5
alpha1=0.61
for (i in 1:200){
  AR1=c(AR1,AR1[i]*alpha1+ruido[i]+alpha)
}  

plot(AR1,type="l")
acf(AR1)
pacf(AR1)

## -------------------------------------------------------------------------

##### 4. Bloque de Modelos Medias Moviles MA #####

set.seed(12345)
ruido=rnorm(200,mean=0,sd=2)
MA1=ruido[1]
beta1=-0.61
for (i in 2:200){
  MA1=c(MA1,ruido[i]+ruido[i-1]*beta1)
}  

plot(MA1,type="l")
acf(MA1)
pacf(MA1)

## -------------------------------------------------------------------------

##### 5. Bloque de Modelos Paseo Aleatorio RW #####

set.seed(12345)
ruido=rnorm(200,mean=0,sd=2)
RW=c(5)
for (i in 1:200){
  RW=c(RW,RW[i]+ruido[i])
}  

plot(RW,type="l")
acf(RW)
pacf(RW)

## -------------------------------------------------------------------------

##### 6. Bloque de ejemplo de análisis #####

plot(AirPassengers)
Air_ts=ts(AirPassengers,frequency=12)

plot(Air_ts)
acf(Air_ts,lag.max=48)
pacf(Air_ts,lag.max=48)
tsdisplay(Air_ts)

Air_ts_d1=diff(Air_ts, lag=1,differences=1)
plot(Air_ts_d1)
acf(Air_ts_d1,lag.max=48)
pacf(Air_ts_d1,lag.max=48)
tsdisplay(Air_ts_d1)

Air_ts_d1_d12=diff(diff(Air_ts, lag=1,differences=1),lag=12,differences=1)
plot(Air_ts_d1_d12)
acf(Air_ts_d1_d12,lag.max=48)
pacf(Air_ts_d1_d12,lag.max=48)
tsdisplay(Air_ts_d1_d12)
Air_model=arima(Air_ts,order=c(0,1,1),seasonal=list(order = c(0, 1, 0)))
plot(forecast(Air_model,h=24))
summary(Air_model)
tsdisplay(residuals(Air_model))

## -------------------------------------------------------------------------

##### 7. Bloque de modelado automático #####

Auto_Air_model=auto.arima(Air_ts,trace=TRUE)
plot(forecast(Auto_Air_model,h=24))
summary(Auto_Air_model)
tsdisplay(residuals(Auto_Air_model))

## -------------------------------------------------------------------------
##       PARTE 2: Modelización de Ventas (Avanzado Opcional)
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 8. Bloque de carga de datos #####

Ventas=read.csv2("Ventas.csv")
Ventas_A0143=Ventas[Ventas$Producto=="A0143",]

## -------------------------------------------------------------------------

##### 9. Bloque de revisión basica del dataset #####

str(Ventas_A0143)
head(Ventas_A0143)
summary(Ventas_A0143)

## -------------------------------------------------------------------------

##### 10. Bloque de formateo de variables #####

Ventas_A0143$Fecha=as.Date(Ventas_A0143$Fecha)
Ventas_A0143$Producto=as.factor(Ventas_A0143$Producto)

Ventas_A0143$Year=format(Ventas_A0143$Fecha,'%Y')
Ventas_A0143$Year=factor(Ventas_A0143$Year,levels=c("2013","2014","2015"))

Ventas_A0143$Mes=months(Ventas_A0143$Fecha)
Ventas_A0143$Mes=factor(Ventas_A0143$Mes,levels=c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"))

Ventas_A0143$Dia=weekdays(Ventas_A0143$Fecha)
Ventas_A0143$Dia=factor(Ventas_A0143$Dia,levels=c("lunes","martes","miércoles","jueves","viernes","sábado","domingo") )

str(Ventas_A0143)
head(Ventas_A0143)
summary(Ventas_A0143)

## -------------------------------------------------------------------------

##### 11. Bloque de Análisis Gráfico #####

plot(Ventas_A0143$Cantidad, type="l")
plot(Ventas_A0143[,c("Fecha","Cantidad","Year","Mes")])
plot(Ventas_A0143$Cantidad~Ventas_A0143$Year)
plot(Ventas_A0143$Cantidad~Ventas_A0143$Mes)
plot(Ventas_A0143$Cantidad~Ventas_A0143$Dia)

plot(Ventas_A0143$Cantidad, type="l")

## -------------------------------------------------------------------------

##### 12. Bloque de Análisis Gráfico Descriptivo #####

Ventas_A0143=Ventas_A0143[order(Ventas_A0143$Fecha),]
plot(Ventas_A0143$Cantidad, type="l")
plot(Ventas_A0143$Fecha,Ventas_A0143$Cantidad, type="l")

## -------------------------------------------------------------------------

##### 13. Bloque de Análisis Gráfico Serie Temporal #####

Ventas_A0143_Mes=aggregate(Ventas_A0143$Cantidad~Ventas_A0143$Year+Ventas_A0143$Mes,FUN=sum)
head(Ventas_A0143_Mes)
colnames(Ventas_A0143_Mes)=c("Year","Mes","Cantidad")
plot(Ventas_A0143_Mes)

plot(Ventas_A0143_Mes$Cantidad~Ventas_A0143_Mes$Year)
plot(Ventas_A0143_Mes$Cantidad~Ventas_A0143_Mes$Mes)

plot(Ventas_A0143_Mes$Cantidad,type="l")

Ventas_A0143_Mes=Ventas_A0143_Mes[order(Ventas_A0143_Mes$Year,Ventas_A0143_Mes$Mes ),]

Ventas_ts <- ts(Ventas_A0143_Mes$Cantidad,start=c(2013,1),frequency= 12)
plot(Ventas_ts)
print(Ventas_ts)

## -------------------------------------------------------------------------

##### 14. Bloque de Descomposición de la Serie #####

Mul_Desc=decompose(Ventas_ts,type="multiplicative")
Add_Desc=decompose(Ventas_ts,type="additive")
plot(Mul_Desc)
plot(Add_Desc)
Mul_Desc
Add_Desc

## -------------------------------------------------------------------------

##### 15. Bloque de Modelo ARIMA #####

plot(Ventas_ts)

acf(Ventas_ts)
pacf(Ventas_ts)

Ventas_ts_d1=diff(Ventas_ts, lag=1,differences=1)
plot(Ventas_ts_d1)
acf(Ventas_ts_d1,lag.max=36)
pacf(Ventas_ts_d1,lag.max=36)

Ventas_ts_d1_d12=diff(diff(Ventas_ts, lag=1,differences=1),lag=12,differences=1)
plot(Ventas_ts_d1_d12)
acf(Ventas_ts_d1_d12,lag.max=36)
pacf(Ventas_ts_d1_d12,lag.max=36)

Ventas_arima=arima(Ventas_ts,order=c(0,1,1),seasonal=list(order = c(0, 1, 0)))
plot(forecast(Ventas_arima,h=24))
summary(Ventas_arima)
tsdisplay(residuals(Ventas_arima))

## -------------------------------------------------------------------------

##### 16. Bloque de Modelo Auto Arima #####

model_arima=auto.arima(Ventas_ts,seasonal=TRUE,trace=TRUE)
plot(forecast(model_arima,h=24))
summary(model_arima)


## -------------------------------------------------------------------------

##### 17. Bloque de Modelo Media Movil #####

model_ma=ma(Ventas_ts,order=3)
summary(model_ma)
plot(forecast(model_ma, fan=TRUE,h=24))
forecast(model_ma, level=c(80,95),h=24)

## -------------------------------------------------------------------------

##### 18. Bloque de Modelo Holt-Winters #####

model_hw=HoltWinters(Ventas_ts)
summary(model_hw)
plot(forecast(model_hw, fan=TRUE,h=24))
forecast(model_hw,level=c(80,95),h=24)

## -------------------------------------------------------------------------

##### 19. Bloque de Modelo Lineal #####

model_tslm=tslm(Ventas_ts~trend + season,data=Ventas_ts)
summary(model_tslm)
plot(forecast(model_tslm, fan=TRUE,h=24))
forecast(model_tslm,level=c(80,95),h=24)

## -------------------------------------------------------------------------