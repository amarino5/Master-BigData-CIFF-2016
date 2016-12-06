## -------------------------------------------------------------------------
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias y establecimiento de directorio #####

if (!require("forecast")){
  install.packages("forecast") 
  library(forecast)
}

##### 2. Bloque de Modelos Estacionarios #####
data = read.csv('monthly-milk-production-pounds-p.csv',header = TRUE, skip=0, sep = ";")
class(data)
data$Year <- format(as.Date(paste0(as.character(data[,1]),"-01"),format = "%Y-%m-%d"),"%Y")
data$Month <- format(as.Date(paste0(as.character(data[,1]),"-01"),format = "%Y-%m-%d"),"%b")
colnames(data) <- c("month","milk","year")
data <- na.omit(data)
head(data)
min(data$year)
milk_ts=ts(data$milk,start=1962,frequency=12)
milk_ts
## -------------------------------------------------------------------------
plot(milk_ts)
acf(milk_ts,lag.max=48)
pacf(milk_ts,lag.max=48)
tsdisplay(milk_ts)

#calculamos las diferencias y ploteamos
milk_ts_d1=diff(milk_ts,lag=1,differences=1)
plot(milk_ts_d1)
acf(milk_ts_d1,lag.max=48)
pacf(milk_ts_d1,lag.max=48)
tsdisplay(milk_ts_d1)

milk_ts_d1_d12=diff(diff(milk_ts, lag=1,differences=1),lag=12,differences=1)
plot(milk_ts_d1_d12)
acf(milk_ts_d1_d12,lag.max=48)
pacf(milk_ts_d1_d12,lag.max=48)
tsdisplay(milk_ts_d1_d12)

milk_model=arima(milk_ts,order=c(0,1,1),seasonal=list(order = c(0, 1, 0)))
plot(forecast(milk_model,h=24))
summary(milk_model)
tsdisplay(residuals(milk_model))