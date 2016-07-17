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

vectorSex <- c("F","M")
dfSex <- data.frame(vectorSex)
for (i in 1:ncol(diabetesdfwork)) {
  if (is.numeric(diabetesdfwork[[i]])){
    temp <- tapply(diabetesdfwork[[i]],diabetesdfwork$SEX,mean )
    tempdf <- data.frame(temp)
    names(tempdf)[1] <- names(diabetesdfwork)[i]
    dfSex <- cbind(dfSex,tempdf[1])
  }
}
head(dfSex)

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
for (col in colnames(diabdfcategorical[,-2])) { #Como la columna sexo es categorica no la contamos
  indBajo <- mean(diabdfcategorical[[col]])-(3*mad(diabdfcategorical[[col]]))
  indSuperior <- mean(diabdfcategorical[[col]])+(3*mad(diabdfcategorical[[col]]))
  Outlier <- diabdfcategorical[ (diabdfcategorical[[col]]<indBajo) | (diabdfcategorical[[col]]> indSuperior) ,] 
  diabdfcategorical[ (diabdfcategorical[[col]]<indBajo) | (diabdfcategorical[[col]]> indSuperior) ,] <- NA
  diabdfcategorical <- na.omit( diabdfcategorical )
}

#separar los datasets en dos fragmentos
indices<-sample(1:nrow(diabdfcategorical),nrow(diabdfcategorical)*70/100)
train<-diabdfcategorical[indices,]
indices<-sample(1:nrow(diabdfcategorical),nrow(diabdfcategorical)*30/100)
test<-diabdfcategorical[indices,]

#escalar los datos para que tenga media cero y varianza 1
scaled<-scale(train)
head(diabetesdf)

#Realizar un modelo de regresion lineal de la variable de respuesta sobre el resto y ajustarlo por #minimos cuadrados usando unicamente los datos del conjunto de entrenamiento.
regresion_train<-lm(Y~AGE+SEX+BMI+BP+S1+S2+S3+S4+S5+S6, data=train)
YPredicttrain<- predict(regresion_train)

#Calcular el error cuadratico medio de los datos del conjunto de entrenamiento y de los datos del #conjunto de test
ecmtrain=(mean(train$Y-YPredicttrain))^2

regresion_test <- lm(Y ~ AGE + SEX + BMI + BP + S1 + S2 + S3 + S4+ S5 + S6, data=test)
YPredictTest=predict(regresion_test)
ecmtest=(mean(test$Y-YPredictTest))^2

