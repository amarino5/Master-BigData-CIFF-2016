############################ ANALISIS ESTADISTICO - Master BI y BD  ###############################

# Hacer uso del dataset "diamonds" que contendrá¡ el precio (entre otras variables interesantes) de unos 54.000 diamantes.
#
# Objetivo : realizar distintos tipos de análisis estadístico de sus variables para intentar
# averiguar algún tipo de comportamiento oculto aparentemente en los datos. 
#
# Para ello os marco los siguientes pasos: tipos de variables, medidas de posición central, medidas de dispersión, 
# distribución y relación entre ellas, más análisis de regresión
#
# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:
# price: Precio en dolares americanos
# carat: peso del diamante
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm 
# y: ancho en  mm 
# z: profundidad en mm 
# depth: porcentaje total de profundidad 
# table: anchura de la parte superior de diamante con relaci?n al punto m?s ancho 


# Responde cada bloque cubriendo al menos lo indicado:

#Muestra representativa
# Selecciona una muestra representativa para "cut"
library(ggplot2)
dt<-as.data.frame(diamonds)
head(dt)
attach(dt)
#install.packages("caTools")
library(caTools)

#El uso de la funcion sample.split realiza una obtencion del porcentaje indicado de casos siempre con
#una muestra representativa de la variable que se le indique dentro del conjunto total.
set.seed(1000)
train =  sample.split(diamonds$cut, SplitRatio = 0.75)
test = sample.split(diamonds$cut, SplitRatio = 0.25)

#An?lisis de las variables
# An?lisis descriptilvo de las variables: Tipo de variable, distribuci?n y representaci?n

str(diamonds)

boxplot(diamonds$carat)
boxplot(diamonds$cut)
boxplot(diamonds$color)
boxplot(diamonds$clarity)
boxplot(diamonds$depth)
boxplot(diamonds$table)
boxplot(diamonds$price)
boxplot(diamonds$x)
boxplot(diamonds$y)
boxplot(diamonds$z)

# Detecci?n de casos at?picos y su tratamiento
summary(diamonds)
#Podemos crear una funcion que elimine los valores que estén fuera del rango intercuartil, solo que 
#con la variable precio tendremos unproblema porque hay muchos valores que se quedan fuera, conlo que habría
#que tratarlos de manera diferente.
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

diamondsdf <- as.data.frame(diamonds)
head(diamondsdf)

#Inferencia
# Calcula un intervalo de confianza para la media de "carat" y "depth"
hist(diamondsdf$carat)
mean(diamonds$carat)
var(diamondsdf$carat)
############
# Dudas:
# Si la poblacion es mayor de 50, y el test del intervalo de confianza hay que hacerlo 
# con la N(0,1), ¿cual es la funcion a aplicar en R?. Porque aqui se hace con la t-student 
# La distribucion de carat no es normal segun el histograma, por tanto, deberia modificarse
# para aplicar la estimacion por intervalos. ¿correcto?
############
t.test(diamondsdf$carat, alternative='two.sided', mu=0.79)
# data:  diamondsdf$carat
# t = 3.8902, df = 53939, p-value = 0.0001003
# alternative hypothesis: true mean is not equal to 0.79
# 95 percent confidence interval:
#   0.7939395 0.8019400
# sample estimates:
#   mean of x 
# 0.7979397 


hist(diamondsdf$depth)
mean(diamonds$depth)
var(diamondsdf$depth)
nrow(diamondsdf$carat)
t.test(diamondsdf$depth, alternative='two.sided', mu=61.75)
# data:  diamondsdf$depth
# t = -0.096476, df = 53939, p-value = 0.9231
# alternative hypothesis: true mean is not equal to 61.75
# 95 percent confidence interval:
#   61.73731 61.76150
# sample estimates:
#   mean of x 
# 61.7494 

# Formula un test de hip?tesis

#Relaciones entre las variables
# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlaci?n)
######################
#El primer problema es que hay variables que son factores, por lo que para calcular la correlacion
#será necesario trasnformarlas en valores numericos.
#
#####################
str(diamondsdf)
diamondsdf['cutNum'] <- as.numeric(diamondsdf$cut)
diamondsdf['colorNum'] <- as.numeric(diamondsdf$color)
diamondsdf['clarityNum'] <- as.numeric(diamondsdf$clarity)
filter <- c('carat','cutNum','colorNum','clarityNum','depth','table','price','x','y','z')
diamondsdfcor <- diamondsdf[filter]
head(diamondsdfcor)
cor(diamondsdfcor)

####################
#Realizariamos el test de Chi cuadrado para probar la hipótesis de que el comportamiento de
#una variable es independiente del comportamiento de la otra. Si queremos ver el comportamiento de todas las variables
#sobre su aportacion al modelo y la relacion parcial entre ellas, podemos ver la informacion que proporciona el propio
#modelo
####################
cov(diamondsdfcor)
parcor(cov(diamondsdfcor))
####################
#                  carat      cutNum      colorNum   clarityNum        depth        table        price           x             y            z
#price       0.665865580  0.09061220 -0.3921653121  0.522873584 -0.071489337 -0.039059622  1.000000000 -0.10667107  0.0090754688 -0.003507143
#
#Parece que la variable mas relacionada con el precio es el peso del mismo, seguido de la claridady el color. Parece sin embargo que el corte 
#no parece significativo a la hora de estimar el precio en una regresion lineal multiple.
#La covarianza parcial negativa, indica que el color afecta inversamente al modelo.
####################


#An?lisis de regresi?n
# Formular un modelo de regresi?n y analiza los resultados
####################
#  Intentaremos estimar el precio del diamante en mercado en funcion del resto de variables, viendo cual aporta mas a la variable dependiente
####################
attach(diamondsdfcor)
rg <- lm(price~carat+clarityNum+colorNum)
rg
# Muestra los residuos y analiza los resultados
# Aplica una transformaci?n a la regresi?n y analiza los resultados
# Interpreta los coeficientes estandarizados de la regresi?n


