#Cargamos las librerias a utilizar
library(FactoMineR)
library(rattle)

#Cargamos los datos
setwd("~/Data Science/Tarea3")

#Lectura de los datos 
data <- read.csv("Servicio_al_cliente.csv", header=TRUE, row.names=1, dec=",", sep=";")

#Informacion acerca de los datos
summary(data)

#Dimensiones del data frame obtenido
dim(data)

#Matriz de correlacion de las variables
cor(data)

#Analisis de componentes principales
componentes <- PCA(data, graph=FALSE)

#Grafico del circulo de correlaciones, sobre las 2 componentes principales
plot(componentes, axes=c(1,2), choix="var", new.plot=TRUE)

#Grafico del circulo de correlaciones, sobre las 2 componentes principales
plot(componentes, axes=c(1,2), choix="ind", new.plot=TRUE)

#Porcentaje de la suma de cosenos cuadrados de los componentes principales 1 y 2 para determinar el grado de representacion de los individuos y las variables sobre estos planos.
ind <- (componentes$ind$cos2[, 1] + componentes$ind$cos2[, 2]) * 100
ind

var <- (componentes$var$cos2[, 1] + componentes$var$cos2[, 2]) * 100
var

#Grafica de los individuos que tengan cos>=0.5
plot(componentes, axes=c(1,2), choix="ind", new.plot=TRUE, select="cos2 0.1")

#Grafica de las variables que tengan cos>=0.5
plot(componentes, axes=c(1,2), choix="var", new.plot=TRUE, select="cos2 0.3")

modelo = hclust(dist(componentes$ind$coord[,c("Dim.1", "Dim.2")]), method="ward.D")
plot(modelo)
rect.hclust(modelo, k = 4, border="blue")

grupos <- cutree(modelo, k = 4)
data$Grupo <- grupos
write.csv(x=data, file="Servicio_al_cliente_sal.csv",row.names=TRUE )

plot(componentes, axes=c(1,2), choix="ind", new.plot=TRUE, col.ind = data$Grupo + 1)
points(data, col = data$Grupo + 1, pch = 19)

