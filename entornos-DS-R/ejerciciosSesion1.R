##Ejercicio 1
n<-250
x<- sample(1:1000,n,replace=T)
y<- sample(1:1000,n,replace=T)
max(x)
max(y)
min(x)
min(y)
mean(x)
mean(y)
length(x[x%%2==0])
##Ejercicio 2
order(x)
sort(x)
y[y<600]
##Revisar porque no me acuerdo.
vector<-c(x[n-2]+2*x[n-1]-x[n])
length(vector)
v<-c(1:cos(y[n])/cos(x[n+2]))
length(v)

##Ejercicio del DataFrame
#1. Previsualizar el contenido con la funci ́on head().
head(mtcars)
#2. Mirar el n ́umero de filas y columnas con nrow() y ncol().
nrow(mtcars)
ncol(mtcars)
#3. Crear un nuevo data frame con los modelos de coche que
#consumen menos de 15 millas/gal ́on.
lessConsume<-mtcars[mtcars$mpg<15,]
#4. Ordenar el data frame anterior por disp.
lessConsume[order(lessConsume$disp),]

#5. Calcular la media de las marchas (gear) de los modelos del
#data frame anterior.
mean(lessConsume[,c("gear")])
colMeans(lessConsume)["gear"]
#Cambiar los nombres de las variables del data frame a var1, etc
names(lessConsume)<-paste0(c("var"),1:11)

##Ejercicio 1 del dataFrame
# Con el data frame iris (viene cargado en R).
# 1. ¿Como est ́a estructurado el data frame? (utilizar las                 
#     funciones str() y dim()).
dim(iris)
str(iris)
names(iris)
# 2. ¿De qu ́e tipo es cada una de las variables del data frame?.
#Las cuatro primeras columnas son numericas y la ultima es un factor, que categoriza cada una de las especies

# 3. Utilizar la funci ́on summary() para obtener un resumen de
# los estad ́ısticos de las variables.
summary(iris)
# 4. Comprobar con las funciones mean(), range(), que se
# obtienen los mismos valores.
mean(iris$Sepal.Length)
range(iris$Sepal.Length)

# 5. Cambia los valores de las variables Sepal.Length
# Sepal.Width de las 5 primeras observaciones por NA.
iris[1:5,"Sepal.Length"] <- NA


# 6. ¿Qu ́e pasa si usamos ahora las funciones mean(), range()
# con las variables Sepal.Length y Sepal.Width? ¿Tiene el
# mismo problema la funci ́on summary()?
mean(iris$Sepal.Length)
range(iris$Sepal.Length)
summary(iris)
#El resultado es que summary interpreta los valores NA y hace los calculos con
#los valores restantes. mean() y range() al contener datos con valores no numericos
#no pueden calcular el resultado

# 7. Ver la documentaci ́on de mean(), range(), etc. ¿Qu ́e
# par ́ametro habr ́ıa que cambiar para arreglar el problema
# anterior?.
mean(iris$Sepal.Length,na.rm = TRUE)
range(iris$Sepal.Length,na.rm = TRUE)
# 8. Visto lo anterior, ¿por qu ́e es importante codificar los
# missing values como NA y no como 0, por ejemplo?
# Si se codifican como 0, el resultado de las operaciones de cómputo pueden no ser correctas

# 9. Eliminar los valores NA usando na.omit().
na.omit(iris)
dim(na.omit(iris))
 
# 10. Calcular la media de la variable Petal.Length para cada
# uno de las distintas especies (Species). Pista: usar la
# funci ́on tapply().
?tapply
tapply(iris$Petal.Length, iris$Species,mean)


