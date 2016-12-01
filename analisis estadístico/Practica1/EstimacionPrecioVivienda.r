
data=read.csv("house_train.csv",stringsAsFactors = FALSE)

##### 3. Bloque de revisi√≥n basica del dataset #####

str(data)
head(data)
summary(data)

##### 4. Bloque de formateo de variables #####
##Cambiamos el formato de la fecha por si es necesario
data$date= as.Date(substr(data$date, 0, 8), "%Y%m%d")
##Nos quedamos con el aÒo y lo trataremos como un factor, ya que puede aportar informacion a la valoracion
data$date = as.factor(format(data$date,'%Y'))
##Vemos que el aÒo de renovacion el valo m·ximo es 2015 y la media 83.11. no tiene mucho sentido este dato
##xipcode es la localizacion. Podria ser tratado como un factor, pero hay demasiados. Puede inflir bastante.
##teniendo el zipcode, la latitud y la longitud al ser coordenadas, podrÌamos utilizarlas pero para hilar muy fino. 
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

##analizamos las condiciones para una regresion linear
##vemos el comporatamiento de las variables. No tiene pinta de ser normal, pero si una poisson
hist(houses$price)
#La plotear la variable dependiente a estimar, junto con su relacion con la superficie, parece que si hay una relacion
#lineal a pesar de haber outliers
plot(houses$price,houses$sqft_living)
#No asi con la superficie de la parcela
plot(houses$price,houses$sqft_lot)


##### 5. Bloque de modelo de regresi√≥n lineal #####
## Estimaci√≥n de ventas en funci√≥n al precio
modelo1=lm(price~sqft_living,data=houses)
summary(modelo1)
#Segun este modelo, solo soy capaz de estimar el 5% de los resultados, por lo que tengo mucha variabilidad
#aunque la variable si aporte al modelo

###Analizamos los residos ########

# modelo0=lm(Cantidad~1,data=Ventas)
# summary(modelo0)
# sd(Ventas$Cantidad)
# F=(sd(Ventas$Cantidad)^2-192.5^2)*(2188-1)/(192.5^2)
plot(modelo1$residuals) 
#vemos que el comportamiento es bueno, aunque hay muchos valores dispersos por la parte alta
smoothScatter(modelo1$residuals)
# Se acercan a una normal muy picuda, es decir, que est·n muy concentrados.
hist(modelo1$residuals)
#Vemos que por el qqnorm en los valores extremos, no podemos explicar el comportamiento.
qqnorm(modelo1$residuals); qqline(modelo1$residuals,col=2)

#Se me ocurre en este punto que:
   # debemos aplicar una transformacion al precio, ya que no cumple la hipotesis de normalidad
   # tendremos que aplicar una rlm asi como suavizar los bordes, ya que tenemos problemas con los valores extremos

## ----------------No hemos detectado cambios estructurales-----------------------------

#aplicamos la transformacion y comprobamos
modelo2=lm(log(price)~sqft_living,data=houses)
summary(modelo2)
plot(modelo2$residuals)
smoothScatter(modelo2$residuals)
hist(modelo2$residuals)
qqnorm(modelo2$residuals); qqline(modelo2$residuals,col=2)
confint(modelo2,level=0.95)

##vemos las correlaciones parciales de las variables para intuir aquellas que puedan ser colineales
library(ggm)
library(igraph)
cor(houses)
str(houses_cor)

houses_cor <- houses
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
cor(houses_cor)
#library(parcor)
#parcor(houses)

modelo3=lm(log(price)~1,data=houses)
summary(modelo3)
plot(modelo3$residuals)
smoothScatter(modelo3$residuals)
hist(modelo3$residuals)
qqnorm(modelo3$residuals); qqline(modelo3$residuals,col=2)
confint(modelo3,level=0.95)

##Al ejectarlo vemos que la variable sqft_basement tiene un N/A por lo que aporta nada
modelo4=lm(log(price)~sqft_living+grade+sqft_above+sqft_living15,data=houses)
summary(modelo4)
plot(modelo4$residuals)
smoothScatter(modelo4$residuals)
hist(modelo4$residuals)
qqnorm(modelo4$residuals); qqline(modelo4$residuals,col=2)
confint(modelo4,level=0.95)

##Ejecutamos un modelo con todas las variables para ver cuales aportan mas o son mas significativas
##Como detectar el overfitting
str(houses)
modelo5=lm(log(price)~sqft_living+bedrooms+sqft_lot+floors+waterfront+view+condition+yr_built+yr_renovated+zipcode+grade+sqft_above+sqft_living15+sqft_lot15,data=houses)
summary(modelo5)
plot(modelo5$residuals)
smoothScatter(modelo5$residuals)
hist(modelo5$residuals)
qqnorm(modelo5$residuals); qqline(modelo5$residuals,col=2)
confint(modelo5,level=0.95)

modelo6=lm(log(price)~sqft_living+sqft_lot+floors+waterfront+view+condition+zipcode+grade+sqft_above+sqft_living15,data=houses)
summary(modelo6)
plot(modelo6$residuals)
smoothScatter(modelo6$residuals)
hist(modelo6$residuals)
qqnorm(modelo6$residuals); qqline(modelo6$residuals,col=2)
confint(modelo6,level=0.95)

#Vemos que el modelo mejora. Trataremos de incluir mas variables para ver si podemos reducir la variabilidad
# Ademas intentaremos aplicar rlm 

##### 8. Bloque de c√°lculo de estad√?sticos robustos #####

alphas=seq(from=0,to=0.5,by=0.01)
medias_recortadas=c()
medias_winsorizadas=c()
for (alpha in alphas){
  medias_recortadas=c(medias_recortadas,mean(houses$price, trim=alpha))
  medias_winsorizadas=c(medias_winsorizadas,MediaWinsor(houses$price,probs=c(alpha,1-alpha)))
}

Estimadores=data.frame(alphas)
Estimadores$media=mean(houses$price)
Estimadores$mediana=median(houses$price)
Estimadores$recortada=medias_recortadas
Estimadores$winsorizada=medias_winsorizadas

Estimadores

plot(Estimadores$recortada~Estimadores$alphas,ylim=c(min(Estimadores[,2:5]),max(Estimadores[,2:5])),col="red",type="l", main="representaci√≥n de medias robustas", xlab="alfas",ylab="precio")
lines(alphas,Estimadores$winsorizada,col="blue")
lines(alphas,Estimadores$media,col="orange")
lines(alphas,Estimadores$mediana,col="green")
legend(x=mean(alphas),y=max(Estimadores[,2:5]),legend=c("media","mediana","media recortada","media winsorizada"),col=c("orange","green","red","blue"),pch=20)

## -------------------------------------------------------------------------

#Generamos el modelo de estadistica robusta y comparamos
###No se porque no me deja ejecutar el modelo robusto con las mismas variables incluidas anteriormente
modelo5=rlm(log(price)~1,data=houses)
summary(modelo5)
plot(modelo5$residuals)
smoothScatter(modelo5$residuals)
hist(modelo5$residuals)
qqnorm(modelo5$residuals); qqline(modelo5$residuals,col=2)
confint(modelo5,level=0.95)


