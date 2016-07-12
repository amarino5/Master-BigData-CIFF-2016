#Proyecto final entornos DS (R)
library(ggplot2)

#Cargar los datos en R.
#Eliminar los missing values, que estan codicados como -9999.00.
setwd("/home/osboxes/app/Master-BigData-CIFF-2016/entornos-DS-R")
diabetesdf <- read.table('diabetes.data',header=TRUE,na.strings = '-9999.0',skip=0)
#diabetesdf[diabetesdf=='-9999.0'] <- NA


diabetesdfwork<-na.omit(diabetesdf)
which(is.na(diabetesdfwork))
#Ver el tipo de cada una de las variables.
str(diabetesdfwork)

#Realizar un analisis estadIstico de las variables
summary(diabetesdfwork)
head(diabetesdfwork)
head(diabetesdf)

#Hacer un GRAFICO de cajas (boxplot) donde se pueda ver la informacion anterior de #forma GRAFICA
#boxplot(diabetesdfwork, use.cols = TRUE)
ggplot(stack(diabetesdfwork), aes(x = ind, y = values)) +
  geom_boxplot()

#Calcular la media para las columnaslas que tienen SEX=M y la
#media para las columnaslas que tienen SEX=F, utilizando la
#funcion tapply
#Paracada columna
tapply(diabetesdfwork$AGE, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$BMI, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$BP, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S1, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S2, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S3, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S4, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S5, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S6, diabetesdfwork$SEX=='M',mean)
#Para todas las columnas
tapply(diabetesdfwork[,1],diabetesdfwork$SEX, mean)

#Calcular la correlacion de todas las variables numericas con
#la variable Y
cor(diabetesdfwork[-2],use="everything")

#grafica con la mayor correlacion
ggplot(diabetesdfwork, aes(x = Y, y = BMI)) +  geom_point(shape=1) 

#grafica con la menor correlacion
ggplot(diabetesdfwork, aes(x = Y, y = S2)) +  geom_point(shape=1) 

#grafica si la correlacion fuese =1. Lo que pasaría es que todos los puntos estarían en la recta de regresion básica por defecto
ggplot(diabetesdfwork, aes(x = Y, y = Y)) +  geom_point(shape=1) 

#cmabiar la variable sex a numerica para poder obtener los quantiles
diabdfcategorical<-diabetesdfwork
dataframe<-head(diabdfcategorical)
diabdfcategorical$SEX<-factor(as.numeric(diabdfcategorical$SEX))
diabdfcategorical$SEX<-as.numeric(diabdfcategorical$SEX)
str(diabdfcategorical)

# ploteamos las variales para detectar posibles desviaciones de outliers
ggplot(stack(diabdfcategorical), aes(x = ind, y = values)) +
  geom_boxplot()

#Funcion que detecta y elimina los outliers con el rango intercuartil sobre el dataset sin variables categoricas
outliersf <- function(dataframe){
  columns<-colnames(dataframe)
  for (xcol in columns) {
    quantiles<-quantile(dataframe[[xcol]])
    print(quantiles)
    q3<-quantiles[4]
    q1<-quantiles[2]
    irange<-3*(quantiles[4]-quantiles[2])
    dataframe[dataframe[[xcol]] > q3+irange,xcol] 
    dataframe[dataframe[[xcol]] < q1-irange,xcol]  
    print(sum(is.na(dataframe)))
    na.omit(dataframe)
  }
}

#Limpiamos el dataset con el MAD
medianas<-lapply(diabetesdfwork,median)
mads<-lapply(diabetesdfwork, mad)
vec_out_inf<-c() #vamos a guardar los límites inferiores para considerar outliers
vec_out_sup<-c() #vamos a guardar los límites superiores para considerar outliers
for (i in 1:length(medianas)){
   vec_out_sup[i]<-medianas[[i]]+ (3*(mads[[i]]))
   vec_out_inf[i]<-medianas[[i]]- (3*(mads[[i]]))
   }
# Eliminamos los registros que tienen alguna variable con valor superior o inferior a los valores tomados como límite de outliers
diabetes_sin_out<-diabetesdfwork
for (i in 1:length(diabetes_sin_out[1,])){
   if(mads[[i]]>0){  
   diabetes_sin_out<-subset(diabetes_sin_out, diabetes_sin_out[,i]>=vec_out_inf[i] & diabetes_sin_out[,i]<=vec_out_sup[i])
   }
}

#separar los datasets en dos fragmentos
indices<-sample(1:nrow(diabdfcategorical),nrow(diabdfcategorical)*70/100)
train<-diabdfcategorical[indices,]
indices<-sample(1:nrow(diabdfcategorical),nrow(diabdfcategorical)*30/100)
test<-diabdfcategorical[indices,]

#escalar los datos para que tenga media cero y varianza 1
escale_dataset<-function(dataframe){
  columns<-colnames(dataframe)
  for (xcol in columns) {
    newdataframe<-apply(dataframe,c(1,2),function(x,y) (x-mean(y))/sd(y),y=xcol )
  }
  
  return(newdataframe)
}
#escalar los datos para que tenga media cero y varianza 1
scaled<-scale(train)
head(diabetesdf)

#Realizar un modelo de regresion lineal de la variable de respuesta sobre el resto y ajustarlo por #minimos cuadrados usando unicamente los datos del conjunto de entrenamiento.
l_regr<-lm(Y~AGE+SEX+BMI+BP+S1+S2+S3+S4+S5+S6, data=train)
Y<- predict(l_regr)

#Calcular el error cuadratico medio de los datos del conjunto de entrenamiento y de los datos del #conjunto de test
err_cuad_medio<-(sum((train$Y-Y_esperado)^2))/dim(train)[1]

