#Con el dataset diamonds intentar reproducir el gráfico:
head(diamonds)
#cargar la libreria ggplot
library(ggplot2)
#creamos un subconjunto de datos
indices <- sample(1:length(diamonds$carat),100)
test <- diamonds[indices,]
head(test)
#ploteamos el conjunto de datos y la regresion lineal asociada
p<-ggplot(test,aes(x=carat,y=price, color=color))
p+geom_point()+xlim(min(test$carat),max(test$carat))+ylim(min(test$price),max(test$price))+geom_smooth(method='lm', formula = y~x, aes(group=1))

