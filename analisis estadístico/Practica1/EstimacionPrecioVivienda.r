
data=read.csv("house_train.csv",stringsAsFactors = FALSE)

##### 3. Bloque de revisi贸n basica del dataset #####

str(data)
head(data)
summary(data)

##### 4. Bloque de formateo de variables #####
##Cambiamos el formato de la fecha por si es necesario
data$date= as.Date(substr(data$date, 0, 8), "%Y%m%d")
##Nos quedamos con el a駉 y lo trataremos como un factor, ya que puede aportar informacion a la valoracion
data$date = as.factor(format(data$date,'%Y'))
##Vemos que el a駉 de renovacion el valo m醲imo es 2015 y la media 83.11. no tiene mucho sentido este dato
##xipcode es la localizacion. Podria ser tratado como un factor, pero hay demasiados. Puede inflir bastante.
##teniendo el zipcode, la latitud y la longitud al ser coordenadas, podr韆mos utilizarlas pero para hilar muy fino. 
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


##### 5. Bloque de modelo de regresi贸n lineal #####
## Estimaci贸n de ventas en funci贸n al precio
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
# Se acercan a una normal muy picuda, es decir, que est醤 muy concentrados.
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

#Vemos que el modelo mejora. Trataremos de incluir mas variables para ver si podemos reducir la variabilidad