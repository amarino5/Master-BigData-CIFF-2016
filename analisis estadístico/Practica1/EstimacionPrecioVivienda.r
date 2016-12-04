######
# Tenemos doa objetivo básicos en la práctica:
# 1.- Analizar el efecto de la superficie de la vivienda en el precio de la vivienda
# 2.- Estimar el precio de venta de unos inmuebles de la cartera de la empresa.
#
# 1. En el primer caso nos interesa ver mas el comportamiento de las variables entre si, 
#   para versu relacion con el precio. Es una análisis mas estadístico (nos interesa una mayor explicacion
#    de la relacion de las variables asi como de la variabilidad de los errores del modelo)
# 2. En el segundo caso nos interesa ser mas efectivos en la prediccion del precio, es un 
#   análisis mas enfocado al ML. Nos interesará mas el ajuste de los errores.



#--------------------------------------------------------------------------------------------
# Inicializamos el dataset
#--------------------------------------------------------------------------------------------

data=read.csv("house_train.csv",stringsAsFactors = FALSE)

#--------------------------------------------------------------------------------------------
# Revisamos los datos y los estadisticos de cada feature del dataset
#--------------------------------------------------------------------------------------------

str(data)
head(data)
summary(data)

#--------------------------------------------------------------------------------------------
# Revisamos y cambiamos el formato de las variables donde lo creemos necesario
#--------------------------------------------------------------------------------------------

##Cambiamos el formato de la fecha por si es necesario
data$date= as.Date(substr(data$date, 0, 8), "%Y%m%d")
##Nos quedamos con el año y lo trataremos como un factor, ya que puede aportar informacion a la valoracion
data$date = as.factor(format(data$date,'%Y'))
##Vemos que el año de renovacion el valo máximo es 2015 y la media 83.11. no tiene mucho sentido este dato
##xipcode es la localizacion. Podria ser tratado como un factor, pero hay demasiados. Puede inflir bastante.
##teniendo el zipcode, la latitud y la longitud al ser coordenadas, podríamos utilizarlas pero para hilar muy fino. 
## de momento las dejamos fuera
#el id no deja de ser un campo de etiqueta, no aporta informacion
data$zipcode = as.factor(data$zipcode)
##modificamos el resto de variables que no son numericas, sino factores, donde el orden numerico no es el significado real de la variable
data$yr_renovated = as.factor(data$yr_renovated)
data$yr_built = as.factor(data$yr_built)
data$condition = as.factor(data$condition)
data$floors = as.factor(data$floors)
data$bathrooms = as.factor(data$bathrooms)
data$bedrooms = as.factor(data$bedrooms)
data$view = as.factor(data$view)
data$waterfront = as.factor(data$waterfront)
unique(data$grade)
data$grade = as.factor(data$grade)
#seleccionamos pues las variables que consideramos optimas
houses = subset(data, select=c("date","price","bedrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","grade","sqft_above","sqft_basement","yr_built","yr_renovated","zipcode","sqft_living15","sqft_lot15"))

str(houses)
head(houses)
summary(houses)

#--------------------------------------------------------------------------------------------
# Dividimos entre train y test, ya que el segundo fichero de la práctica no da datos de precios
#--------------------------------------------------------------------------------------------

sample = sample.split(houses$price, SplitRatio = 0.5)
house_train = subset(houses, SAMPLE == TRUE)
house_test = subset(houses, SAMPLE == FALSE)

#--------------------------------------------------------------------------------------------
# Analizamos el comportamiento de la variable dependiente a estudiar (precio) para ver si cumple
# las condiciones de normalidad (etc) para aplicar un modelo de regresion
#--------------------------------------------------------------------------------------------

##vemos el comporatamiento de las variables. No tiene pinta de ser normal, pero si una poisson
hist(house_train$price)
#La plotear la variable dependiente a estimar, junto con su relacion con la superficie, parece que si hay una relacion
#lineal a pesar de haber outliers
plot(house_train$price,house_train$sqft_living)
#No asi con la superficie de la parcela
plot(house_train$price,house_train$sqft_lot)


#--------------------------------------------------------------------------------------------
# Aplicamos un modelo de regresion lineal para ver la relacion entre precio y metros cuadrados
# y estimar su comportamiento en base los parámetros del modelo y los residuos
#--------------------------------------------------------------------------------------------

## EstimaciÃ³n de ventas en funciÃ³n al precio
modelo1=lm(price~sqft_living,data=house_train)
summary(modelo1)
#Segun este modelo, solo soy capaz de estimar el 5% de los resultados, por lo que tengo mucha variabilidad
#aunque la variable si aporte al modelo
plot(modelo1$residuals) 
#vemos que el comportamiento es bueno, aunque hay muchos valores dispersos por la parte alta
smoothScatter(modelo1$residuals)
# Se acercan a una normal muy picuda, es decir, que están muy concentrados.
hist(modelo1$residuals)
#Vemos que por el qqnorm en los valores extremos, no podemos explicar el comportamiento.
qqnorm(modelo1$residuals); qqline(modelo1$residuals,col=2)
confint(modelo1,level=0.95)
#----------------------------------------------------
# La relacion entre el precio y la superficie de metros cuadrádos útiles podría describirse como:
# precio = -55.283,995 + 285,540*sqft_living, con un nivel de confianza del 95% de que el factor
# que multimplica sqft_living está entre 279,4704 - 291,6087
# Lo cual quiere decir, que por cada metro cuadrado, umenta aproximadamente entre 280 y 290$ el precio
# eso en este modelo, que no es bueno y no está ajustado.
#----------------------------------------------------


#--------------------------------------------------------------------------------------------
#Se me ocurre en este punto que:
  # debemos aplicar una transformacion al precio, ya que no cumple la hipotesis de normalidad
  # tendremos que aplicar una rlm asi como suavizar los bordes, ya que tenemos problemas con los valores extremos
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#No hemos detectado cambios estructurales extremos
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#Puesto que la variable del precio no sigue valores normales, sino aprarentemente mas una distrbucion 
# de poisson, aplicamos una transformacion logaritmica y comprobamos los resultados. 
#--------------------------------------------------------------------------------------------
#aplicamos la transformacion y comprobamos
modelo2=lm(log(price)~sqft_living,data=house_train)
summary(modelo2)
plot(modelo2$residuals)
smoothScatter(modelo2$residuals)
hist(modelo2$residuals)
qqnorm(modelo2$residuals); qqline(modelo2$residuals,col=2)
confint(modelo2,level=0.95)

#--------------------------------------------------------------------------------------------
# Vemos que el modelo transformado se ajusta mejora por las siguientes razones:
  # Los resuiduos siguen una distribucion normal entre 0 y uno.
  # la gráfica de disperion de los errores está muy centrada en torno al cero
  # la visualizacion del qqplot se ajusta muy bien excepto en los extremos que hay un poco de variacion.

# Como primera conclusión, un modelo transformado ajusta mejor. La variabilidad de los errores (R2 ajustado)
# es practicamente la misma en ambos casos.Aplicamos semielasticidad al modelo (transformacion en una de las variables)
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
# El siguente paso sería calcular las correlaciones parciales, para ver aquellas que mas aportan al modelo
# lo ideal seria utilizar la correlacion parcial, que elimina los efectos de una variable sobre las demas
# evitando asi la multicolinealidad, pero las librerías que he usado no me funcionan, asi que utilizaré
# la correlacion normal, intentado detectar el overfitting con el comportamiento de los residuos.
#--------------------------------------------------------------------------------------------

library(ggm)
library(igraph)
houses_cor <- house_train
houses_cor$date = as.numeric(houses_cor$date)
houses_cor$bedrooms = as.numeric(houses_cor$bedrooms)
houses_cor$floors = as.numeric(houses_cor$floors)
houses_cor$waterfront = as.numeric(houses_cor$waterfront)
houses_cor$view = as.numeric(houses_cor$view)
houses_cor$condition = as.numeric(houses_cor$condition)
houses_cor$grade = as.numeric(houses_cor$grade)
houses_cor$yr_built = as.numeric(houses_cor$yr_built)
houses_cor$yr_renovated = as.numeric(houses_cor$yr_renovated)
houses_cor$zipcode = as.numeric(houses_cor$zipcode)
#library(parcor)
#parcor(houses_cor)


cor(houses_cor)
# Segun la matriz de correlaciones (sin tener en cuenta los efectos de la multicolinealidad),
# las variables que mas aportarían a la estimacion del precio serían:
#sqft_living 0.702916354
#grade 0.667468232
#sqft_above 0.605277522
#sqft_living15 0.583480821
#view 0.391022681

#--------------------------------------------------------------------------------------------
#vamos a analizar la aportacion de las distintas variables al modelo, con varias simulaciones.
# De esta manera, nos permitirá ir reduciendo aquellas que no aporten.
#--------------------------------------------------------------------------------------------
modelo3=lm(log(price)~.,data=house_train)
summary(modelo3)
plot(modelo3$residuals)
smoothScatter(modelo3$residuals)
hist(modelo3$residuals)
qqnorm(modelo3$residuals); qqline(modelo3$residuals,col=2)
confint(modelo3,level=0.95)
# El modelo 3 nos indica cuales de las variables no son significativas para le modelo. Entre ellas
# aquellas que no tienen *: bedrooms, sqft_basement,yr_built, yr_renovated.
# Evidentemente, algunas de las variables anteriores, al ser factores, aportan cuando hay datos, 
# pero no aportan cuando no los hay, con lo cual pueden deshecharse las variables
# En este caso el R2 es elevado R-squared:  0.8866, eso indica que puede explicarse el 88% de la variabilidad
#de los datos.


##Eliminamos del modelo las variables que hemos visto que no aportan.
modelo4=lm(log(price)~sqft_living+grade+sqft_above+sqft_living15+view+waterfront+zipcode,data=house_train)
summary(modelo4)
plot(modelo4$residuals)
smoothScatter(modelo4$residuals)
hist(modelo4$residuals)
qqnorm(modelo4$residuals); qqline(modelo4$residuals,col=2)
confint(modelo4,level=0.95)
#Vemos en este caso, que con muchas menos variables, somos capaces de explicar practicamente la misma
# variabilidad del modelo, sin embargo, el análisis de los residuos, vemos en el qqplot que se nos alejan
# de los extremos, con lo que el grado de predictividad del modelo no es del todo bueno


##Eliminamos la variable del zipcode, el cual como podemos ver en los resultados, explicaba muy bien
#la variabilidad de los resultados, pero hacia que el modelo entrase en overfitting, de manera que ahora 
# los residuos se ajustan mucho mejor a la linea dentro del qqplot. Los residuos también están
# perfectamente ajustados a una normal con media 0.
modelo5=lm(log(price)~sqft_living+grade+sqft_above+sqft_living15+view+waterfront,data=house_train)
summary(modelo5)
plot(modelo5$residuals)
smoothScatter(modelo5$residuals)
hist(modelo5$residuals)
qqnorm(modelo5$residuals); qqline(modelo5$residuals,col=2)
confint(modelo5,level=0.95)



#--------------------------------------------------------------------------------------------
#vamos a intentar ver el comportamiento del modelo que mejor predicie los resultados (modelo 5)
# frente a un modelo robusto y comprarlos, para ver que efectivamente el comportamiento es similar,
# indicativo de que vamos por buen camino.
#--------------------------------------------------------------------------------------------

alphas=seq(from=0,to=0.5,by=0.01)
medias_recortadas=c()
medias_winsorizadas=c()
for (alpha in alphas){
  medias_recortadas=c(medias_recortadas,mean(house_train$price, trim=alpha))
  medias_winsorizadas=c(medias_winsorizadas,MediaWinsor(house_train$price,probs=c(alpha,1-alpha)))
}

Estimadores=data.frame(alphas)
Estimadores$media=mean(house_train$price)
Estimadores$mediana=median(house_train$price)
Estimadores$recortada=medias_recortadas
Estimadores$winsorizada=medias_winsorizadas

Estimadores

plot(Estimadores$recortada~Estimadores$alphas,ylim=c(min(Estimadores[,2:5]),max(Estimadores[,2:5])),col="red",type="l", main="representaciÃ³n de medias robustas", xlab="alfas",ylab="precio")
lines(alphas,Estimadores$winsorizada,col="blue")
lines(alphas,Estimadores$media,col="orange")
lines(alphas,Estimadores$mediana,col="green")
legend(x=mean(alphas),y=max(Estimadores[,2:5]),legend=c("media","mediana","media recortada","media winsorizada"),col=c("orange","green","red","blue"),pch=20)

#--------------------------------------------------------------------------------------------
#Generamos el modelo robusto par comprar
#--------------------------------------------------------------------------------------------
###No se porque no me deja ejecutar el modelo robusto con las mismas variables incluidas anteriormente
modelo6=rlm(log(price)~sqft_living+grade+sqft_above+sqft_living15+view+waterfront,data=house_train)
summary(modelo6)
plot(modelo6$residuals)
smoothScatter(modelo6$residuals)
hist(modelo6$residuals)
qqnorm(modelo6$residuals); qqline(modelo6$residuals,col=2)
confint(modelo6,level=0.95)
#Vemos que el comporamiento de los residuos es idéntico, por lo cual, el modelo5 o el modelo6 son óptimos
# para hacer cualquier prediccion futura del precio respecto a las variables independientes encontradas.

#--------------------------------------------------------------------------------------------
# Evaluacion del modelo
#--------------------------------------------------------------------------------------------

house_train$prediccionLM=predict(modelo5,type="response")
R2_Train_LM=1-sum((house_train$price-house_train$prediccionLM)^2)/sum((house_train$price-mean(house_train$price))^2)

house_test$prediccionLM=predict(modelo5,newdata=house_test,type="response")
R2_Test_LM=1-sum((house_test$price-house_test$prediccionLM)^2)/sum((house_test$price-mean(house_test$price))^2)

R2_Train_LM
R2_Test_LM
#Vemos que la diferencia del error en ambos conjuntos es bsatante pequeña, por lo que el 
#comportamiento del modelo es aceptable
#> R2_Train_LM
#[1] -2.046132
#> R2_Test_LM
#[1] -2.217989

#Lo comparamos ahora con los resultados del modelo robusto
house_train$prediccionRLM=predict(modelo6,type="response")
R2_Train_RLM=1-sum((house_train$price-house_train$prediccionRLM)^2)/sum((house_train$price-mean(house_train$price))^2)

house_test$prediccionRLM=predict(modelo6,newdata=house_test,type="response")
R2_Test_RLM=1-sum((house_test$price-house_test$prediccionRLM)^2)/sum((house_test$price-mean(house_test$price))^2)

R2_Train_RLM
R2_Test_RLM

#> R2_Train_RLM
#[1] -2.046132
#> R2_Test_RLM
#[1] -2.217989
#Sale exactamente el mismo error que con el modelo de regresion lineal, por lo cual 
#el comportamiento del modelo es estable.

SCE_LM=sum((house_test$price-house_test$prediccionLM)^2) #MSE
SCE_RLM=sum((house_test$price-house_test$prediccionRLM)^2) #MSE

SCE_LM
SCE_RLM

MAE_LM=sum(abs(house_test$price-house_test$prediccionLM))
MAE_RLM=sum(abs(house_test$price-house_test$prediccionRLM))

MAE_LM
MAE_RLM

#---------------CONCLUSIONES A LOS REQUISITOS 1 y 2-----------------------------------------------------------------------------
# 1. La relacion entre las variables puede decirse lineal, y representada como una ecuacion lineal (explicado anteriormente)
# 2. Puesto que el precio no sigue una distribucion normal, lo mejor es transformarlo, por lo que la
#    mejor relacion entre los factores que explican el precio, viene dada por la formula lineal
#    con los factores del modleo4, que explican un 88% de la variabilidad. La interpretacion de aqui,
#    puesto que se transforma el precio, es que el aumento de 1 unidad en cada variable afecta un % al precio
# 3. El modelo que mejor predice un psible precio es el modelo5, donde los residuos se comportan 
#    mejor, y la comparativa tanto con el modelo robusto, como con el comportamiento entre los
#    conjuntos de train y test, así como las distintas métricas, son muy similares, por tanto es un 
#    modelo estable.
#--------------------------------------------------------------------------------------------
 

