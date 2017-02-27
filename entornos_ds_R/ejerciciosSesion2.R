#1. Generar 10000 números aleatorios con una distribución 
#normal estándar (media 0 y varianza 
data<-rnorm(1000)

#2. Realizar un histograma de los valores anteriores
#¿Cual es el menor y mayor valor generado?
hist(data)
min(data)
max(data)

#3. Generar los valores de la distribución normal teoricos en el intervalo anterior utilizando la funcion dnorm().
##Generamos  una secuencia de números desde el mínimo de los datos generado hasta el máximo en saltos de 0.05 en 0.05
xaxis<-seq(min(data),max(data),0.05)

##Obtenemos los valores de la distribución normal para ese intervalor:
dnorm(xaxis)
#4. Al histograma anterior, superponer una curva con la 
#función de densidad teórica calculada. 
#¿Se aproxima el histograma al valor teórico?
lines(xaxis,dnorm(xaxis))

##No se pinta la línea debido a la escala. Los valores del histograma son muy grandes respecto a los valores que se representan en la línea.

#5. Ver el parámetro probability de la función hist y volver a generar el histograma cambiando su valor. ¿Se aproxima ahora a la función de densidad teórica?
?hist
#Vemos que los parámetros probability y freq son equivalentes:
#Si al parámetro probability le damos el valor TRUE, los valores que se representarán serán las frecuencias, no los totales.
hist(data, probability = TRUE)

#Se ve que el eje y tiene una escala diferente. Ahora podemos pintar la línea sobre el histograma:
lines(xaxis,dnorm(xaxis))
#Vemos que el histograma sigue se aproxima a una distribución normal.

#Ejercicio 24
#genero in intervalo en X para la representacion de los valores de Y
#El 1 es el paso entre coordenadas X
#y es la funcion cuadratica
x<-seq(-4,4,0.11)
y<-x^2
#type para indicar el tipo de union
plot(x,y, type="l")

#ahora pintamos la distribucion normal
y<-dnorm(x)
plot(x,y, type="l")
