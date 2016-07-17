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


library(dplyr)
attach(diamonds)
head(diamonds)

filter(diamonds, cut=='Ideal') 
selected_data<-select(diamonds,carat,cut,color,price,clarity)
head(selected_data)
diamonds%>%mutate(price_carat=price/carat)%>%head 
diamonds%>%mutate(price_carat=price/carat)%>%group_by(color)%>%summarise(avg_price_carat = mean(price_carat))%>%arrange(avg_price_carat,desc(avg_price_carat))
