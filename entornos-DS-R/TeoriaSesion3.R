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

#la funcion line() sirve para crear lineas.