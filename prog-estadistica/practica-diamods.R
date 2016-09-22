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
#install.packages("parcor")
library(caTools)
library(parcor)

#El uso de la funcion sample.split realiza una obtencion del porcentaje indicado de casos siempre con
#una muestra representativa de la variable que se le indique dentro del conjunto total.
set.seed(1000)
train =  sample.split(diamonds$cut, SplitRatio = 0.75)
test = sample.split(diamonds$cut, SplitRatio = 0.25)

#Analisis de las variables
#Analisis descriptilvo de las variables: Tipo de variable, distribuci?n y representaci?n
#############################
# $ carat  : num  :variable numerica.
# $ cut    : Ord.factor w/ 5 levels: Factor con 5 niveles
# $ color  : Ord.factor w/ 7 levels : Factor con 7 niveles
# $ clarity: Ord.factor w/ 8 levels : Factor con 8 niveles
# $ depth  : num  : Numerico flotante
# $ table  : num  :numerico flotante
# $ price  : int  : valores enteros
# $ x      : num  : numerico flotante
# $ y      : num  : numerico flotante
# $ z      : num  : numerico flotante
##############################
str(diamonds)

ExploratoryAnalysis <- function ( data ){
  
  if (is.factor(data)){
    data <- as.numeric(data)
    }
  print(summary(data))
  par(mfrow=c(2,2))
  boxplot(data)
  hist(data)
  
  #analisis de los datos para la normalidad 
  plot(density(data))
  qqnorm(data)
 
  par(mfrow=c(1,1))
}

ExploratoryAnalysis(diamonds$carat)
#############################
# $ carat  : num  :variable numerica. Parece que los valores siguen una distribucion normal, ya que
#la mediana esta centrada, pero tiene muchos valores fuera del tercer cuartil, por lo que pueden 
#o bien considerarse outliers, o darles un tratamiento distinto:(DUDA?)
#############################
ExploratoryAnalysis(diamonds$cut)

#############################
# $ cut    : Ord.factor w/ 5 levels: Factor con 5 niveles. Parece que la distribucion del tipo de corte
#esta centrada y entre los valores 3 y 5
#############################
ExploratoryAnalysis(diamonds$color)
ExploratoryAnalysis(diamonds$clarity)
ExploratoryAnalysis(diamonds$depth)
ExploratoryAnalysis(diamonds$table)
ExploratoryAnalysis(diamonds$price)
ExploratoryAnalysis(diamonds$x)
ExploratoryAnalysis(diamonds$y)
ExploratoryAnalysis(diamonds$z)

# Detecci?n de casos at?picos y su tratamiento
#### Detecci󮠤e casos atcos y su tratamiento
#Tal como revisamos anteriormente, hay variables que tienen muchos valores fuera del rango intercuartilico
#al ser tantos valores, lo ideal seria partir en dos muestras (los que estan en rango) y los que estan
#fuera del rango y hacer los mismos pasos de tratamiento de análisis en subgrupos distintos.

#Como primer paso, eliminamos los valores a 0 de las variables X;Y;Z porque no tiene sentido que 
#esas medidas estén a 0. Otra solución podría ser establecerlos a la media, o bien de la variable 
#o bien de los estratos seleccionados 

diamonds_clean<-diamonds[diamonds$x>0 & diamonds$y>0 & diamonds$z>0,]

# Comprobamos la cantidad de outliers:
out_carat=c(quantile(diamonds$carat,.25)-1.5*IQR(diamonds$carat), quantile(diamonds$carat,.75)+1.5*IQR(diamonds$carat))
out_depth= c(quantile(diamonds$carat,.25)-1.5*IQR(diamonds$depth), quantile(diamonds$depth,.75)+1.5*IQR(diamonds$depth))
out_table= c(quantile(diamonds$table,.25)-1.5*IQR(diamonds$table), quantile(diamonds$table,.75)+1.5*IQR(diamonds$table))
out_price= c(quantile(diamonds$price,.25)-1.5*IQR(diamonds$price), quantile(diamonds$price,.75)+1.5*IQR(diamonds$price))
out_x = c(quantile(diamonds$x,.25)-1.5*IQR(diamonds$x), quantile(diamonds$x,.75)+1.5*IQR(diamonds$x))
out_y = c(quantile(diamonds$y,.25)-1.5*IQR(diamonds$y), quantile(diamonds$y,.75)+1.5*IQR(diamonds$y))
out_z = c(quantile(diamonds$z,.25)-1.5*IQR(diamonds$z), quantile(diamonds$z,.75)+1.5*IQR(diamonds$z))
out<- rbind(out_carat, out_depth, out_table, out_price, out_x, out_y, out_z)

dt_sinOut<-diamonds[diamonds$carat>out[1,1] & diamonds$carat<out[1,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$depth>out[2,1] & dt_sinOut$depth<out[2,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$table>out[3,1] & dt_sinOut$table<out[3,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$price>out[4,1] & dt_sinOut$price<out[4,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$x>out[5,1] & dt_sinOut$x<out[5,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$y>out[6,1] & dt_sinOut$y<out[6,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$z>out[7,1] & dt_sinOut$z<out[7,2],]
#comprobamos el porcentaje de datos que hemos eliminado
porcent=(dim(dt_sinOut)[1]*100)/dim(diamonds)[1] # eliminando los outliers mantenemos el 90.23% de la poblacion. Seguiremos los análisis con este nuevo data set.
diamondsdf<-dt_sinOut

#Inferencia
# Calcula un intervalo de confianza para la media de "carat" y "depth"
hist(diamondsdf$carat)
mean(diamonds$carat)
var(diamondsdf$carat)
############
# DUDA?:
# Si la poblacion es mayor de 50, y el test del intervalo de confianza hay que hacerlo 
# con la N(0,1), ¿cual es la funcion a aplicar en R?. Porque aqui se hace con la t-student 
# La distribucion de carat no es normal segun el histograma, por tanto, deberia modificarse
# para aplicar la estimacion por intervalos. ¿correcto?
############
t.test(diamondsdf$carat, alternative='two.sided', mu=0.79)
# data:  diamondsdf$carat
# t = -47.377, df = 48670, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0.79
# 95 percent confidence interval:
#   0.7073269 0.7138956
# sample estimates:
#   mean of x 
# 0.7106112 


hist(diamondsdf$depth)
mean(diamonds$depth)
var(diamondsdf$depth)
nrow(diamondsdf$carat)
t.test(diamondsdf$depth, alternative='two.sided', mu=61.75)
# data:  diamondsdf$depth
# t = -10.18, df = 48670, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 61.75
# 95 percent confidence interval:
#   61.68094 61.70324
# sample estimates:
#   mean of x 
# 61.69209 

# TEST DE HIPOTESIS
##########################
# Realizaremos un test de hipotesis sobre los valores de la media del precio. Podemos hacerlo sobre
# el conjunto completo y luego sobre el dataset elimado de outliers.
#
# Ho: media precio < o = 4000
# H1: media precio > 4000
# tomamos como la desviacion tipica la de la muestra sigma = 3989
# tomaremos un nivel de significacion alfa = 0,05
# SUPONEMOS que el precio de los diamantes sigue una distribucion normal (QUE NO LA SIGUE)
# Por tanto, nuestra region de aceptacion sería la probabilidad de la normal estandar de 1-alfa= 0,95
# qnorm(c(0.95), mean=0, sd=1, lower.tail=TRUE) =1,644854
# Nuestra region de aceptacion va de -infinito a 1,644854 por lo que abrá que calcular el estadistico
# de la muestra
##########################
Zalfa <- qnorm(c(0.95), mean=0, sd=1, lower.tail=TRUE)
Mu0 = 4000
sd = 3989
media = mean(diamonds$price, na.rm=TRUE)
estadistico = (media - Mu0)/(sd/sqrt(length(diamonds$price))) 
# Puesto que el estadistico nos da un valor de -3.9 
# y el valor de Zalfa que hemos calculado nos da 1.64
# estimamos por tanto que el valor de estadistico cae dentro de la zona de aceptacion y por tanto
# no podemos rechazar H0, por lo que aceptamos dados los datos de la media de la muestra será menor
# de 4000
# Otra manera de hacerlo sería con el t.test
t.test(diamonds$price,alternative='greater', mu=4000, conf.level=.95)
# Cuyos datos aportan muchos resultados y la misma conclusion
#data:  diamonds$price
# t = -3.9121, df = 53939, p-value = 1 -> El p-valor es 1 por lo que practicamente podemos aceptar la evidencia
# alternative hypothesis: true mean is greater than 4000 -> la hipotesis alternativa confirmaria que la media esta por encima de 4000
# 95 percent confidence interval: -> a un nivel de confianza del 95% podemos confirmar que la media es inferior a 4904 dada eta muestra
#   3904.545      Inf
# sample estimates:
#   mean of x 
# 3932.8 


      
#Relaciones entre las variables
# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlaci?n)

######################
#INDEPENDENCIA
#
#Para ver como dos variables son independientes, realizamos el chiq test. 
#realizaremos por ejemplo el test de independencia delas variables que estudiaremos luego
#como el precio y el color
#####################
chisq.test(diamonds$price,diamonds$carat, simulate.p.value=TRUE)

######################
#ANOVA
#Calcularemos el ANOVA de los factores respecto a la variable dependiente por ejemplo de cut respecto a price 
#
#Asumiendo que la variable price sigue una distribucion normal y con varianza comun para los 
#tipos de corte
#La variable price no sigue una distribucion normal puesto que el test de saphiro el p-valor < 0,05
#Los calculos del anova nos dicen que hay diferencias significatvas respecto a las médias
#del tipo de corte respecto al precio, esto viene causado seguramente por numeor de outliers (o precios)
#excesivos en cada tipo (se ve en el boxplot) lo que afecta a las medias y varianzas.
#####################
tapply(diamonds$price, diamonds$cut, summary)
plot(diamonds$cut, diamonds$price)
#realizamos el test de shapito con una muestra de 5000 registros que es el límite que nos permite la funcion
shapiro.test(diamonds$price[1:4999])

.anova <-aov(diamonds$price~diamonds$cut)
summary(.anova)

#A la vista de los resultados del anova, el comportamiento de las medias de las variables difiere 
#significativamente ya que el P-valor es significativamente bajo.
# Df    Sum Sq   Mean Sq F value Pr(>F)    
# diamonds$cut     4 1.104e+10 2.760e+09   175.7 <2e-16 ***
#   Residuals    53935 8.474e+11 1.571e+07                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

######################
#CORRELACION
#El primer problema es que hay variables que son factores, por lo que para calcular la correlacion
#será necesario trasnformarlas en valores numericos.DUDA?cierto?
#
#####################
str(diamondsdf)
diamondsdf['cutNum'] <- as.numeric(diamondsdf$cut)
diamondsdf['colorNum'] <- as.numeric(diamondsdf$color)
diamondsdf['clarityNum'] <- as.numeric(diamondsdf$clarity)
filter <- c('carat','cutNum','colorNum','clarityNum','depth','table','price','x','y','z')
diamondsdfcor <- diamondsdf[,filter]
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


#ANALISIS DE REGRESION
# Formular un modelo de regresi?n y analiza los resultados
####################
#  Intentaremos estimar el precio del diamante en mercado en funcion del resto de variables, viendo cual aporta mas a la variable dependiente
####################
attach(diamondsdf)
rg <- lm(price~carat+clarityNum+colorNum, data=diamondsdf)
summary(rg)
# Muestra los residuos y analiza los resultados
summary(rg)$r.squared
summary(rg)$adj.r.squared
summary(rg)$coeff

####################
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6085.6  -503.8  -134.2   407.6  4909.3 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3225.924     15.702 -205.44   <2e-16 ***
#   carat        7749.755     11.352  682.66   <2e-16 ***
#   clarityNum    401.371      2.473  162.29   <2e-16 ***
#   colorNum     -231.378      2.329  -99.33   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 824.9 on 48667 degrees of freedom
# Multiple R-squared:  0.9095,	Adjusted R-squared:  0.9095 
# F-statistic: 1.63e+05 on 3 and 48667 DF,  p-value: < 2.2e-16

#El R-squared ajustado es el que hay que revisar al ser un modelo de regresion lineal multiple. 
#Puesto que esta en 0.9 es muy elevadopor lo que el modelo parece que se ajusta bastante bien
#El p-valor esta muy por debajo de 0,05 por lo tanto corrobora la bodad de ajuste.
#las tres varibles seleccionadas en el modelo parecen aportar bastante al modelo por los niveles de significacion.
####################
plot(resid(rg))
abline(0,0,col="red")
####################
#La grafica demuestra heterocedasteidad, es decir que el modelo no se ajusta a la distribucion normal
#Ya que los puntos no están alienados conforme a la linea marcada y hay mucha varianza
####################

# Aplica una transformaci?n a la regresi?n y analiza los resultados
rgTrans<-lm(log(price) ~ carat+clarityNum+colorNum, data=diamondsdf)
summary(rgTrans)
plot(resid(rgTrans))
abline(0,0,col="red")

# Interpreta los coeficientes estandarizados de la regresi?n
# Parece que el resultado mejora un poco respecto al modelo normal sin transformación por dos razones fundamentales
#    Por una parte, el p-valor sigue siendo igual de pequeño, por lo que se rechaza la hipotesis nula de que los coeficientes sean iguales a 0
#    Por otra parte, el R2 ajustado es un poco mejor que en el modelo sin normalizacion, con lo que se explican algunos datos mas sobre el modelo
#    Por ultimo, la grafica residual, se ve todavía que los valores se mueven a un lado y a otro de la linea pero con menor varianza
#    de hecho, el valor residual estandar es mucho mas pequeño.

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.6868568  0.0048742  1166.7   <2e-16 ***
#   carat        2.6080737  0.0035239   740.1   <2e-16 ***
#   clarityNum   0.1007502  0.0007677   131.2   <2e-16 ***
#   colorNum    -0.0883116  0.0007231  -122.1   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2561 on 48667 degrees of freedom
# Multiple R-squared:  0.9232,	Adjusted R-squared:  0.9232 
# F-statistic: 1.95e+05 on 3 and 48667 DF,  p-value: < 2.2e-16

shapiro.test(sample(resid(rgTrans),4999))
#TOdavía no pasa el test de normalidad la distribucion de los residuos, con lo que no es buena señal
