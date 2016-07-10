#Proyecto final entornos DS (R)

library(ggplot2)

setwd("/home/osboxes/app/Master-BigData-CIFF-2016/entornos-DS-R")
diabetesdf <- read.table('diabetes.data',header=TRUE,na.strings = '-9999.0',skip=0)
#diabetesdf[diabetesdf=='-9999.0'] <- NA
diabetesdfwork<-na.omit(diabetesdf)
which(is.na(diabetesdfwork))
str(diabetesdfwork)
summary(diabetesdfwork)
head(diabetesdfwork)
head(diabetesdf)
#boxplot(diabetesdfwork, use.cols = TRUE)
ggplot(stack(diabetesdfwork), aes(x = ind, y = values)) +
  geom_boxplot()
tapply(diabetesdfwork$AGE, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$BMI, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$BP, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S1, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S2, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S3, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S4, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S5, diabetesdfwork$SEX=='M',mean)
tapply(diabetesdfwork$S6, diabetesdfwork$SEX=='M',mean)

head(diabetesdfwork)
cor(diabetesdfwork[-2],use="everything")

#<grafica con la mayor correlacion
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

#Funcion que detecta y elimina los outliers sobre el dataset sin variables categoricas
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

#Pasamos la  funcion al dataset
outliersf(diabdfcategorical)
summary(diabdfcategorical)

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
scaled<-scale(train)
head(diabetesdf)
