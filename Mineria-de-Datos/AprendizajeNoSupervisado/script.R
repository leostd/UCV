# Carga de bibliotecas y fuentes ----------------------------------------------------
#Biblioteca para graficos 3D
library(rgl)
#Metodos de mineria de datos
library(FactoMineR)
source("funciones.r")

# Carga de Datasets -------------------------------------------------------
a = read.csv("a.csv", header = F)
a_big = read.csv("a_big.csv", header = F)
good_luck = read.csv("good_luck.csv", header = F)
guess = read.csv("guess.csv", header = F)
h = read.csv("h.csv", header = F)
help = read.csv("help.csv", header = F)
moon = read.csv("moon.csv", header = F)
s = read.csv("s.csv", header = F)


# Agregar Clases a los datasets correspondientes --------------------------
g1 = list(help, s, h)
names = c("help", "s", "h")
layout(1:3)
histograms = list()
clases = list()
j = 1
for ( i in g1 ){
  histograms[[j]] = hist( i$V4, main = names[j],xlab =  "Clase", xlim = range(i$V4))
  clases[[j]] = asignarClase(histograms[[j]]$breaks, i$V4)
  j = j + 1
}
help$clase = clases[[1]]
s$clase = clases[[2]]
h$clase = clases[[3]]
plot3d(help$V1, help$V2, help$V3, col=help$clase, type="s")
readkey()
plot3d(s$V1, s$V2, s$V3, col=s$clase, type="s")
readkey()
plot3d(h$V1, h$V2, h$V3, col=h$clase, type="s")


# Correccion de las clases que empiezan por 0 -----------------------------
g2 = list(a, a_big, good_luck, moon)
a$clase = a$V3 +1
a_big$clase = a_big$V3 + 1
good_luck$clase = good_luck$V11 + 1
moon$clase = moon$V3 + 1

# Analisis Exploratorio - Dataset a.csv -----------------------------------
dim(a)
summary(a)
plot(a[,-c(3,4)], main = "a.csv Dataset", col = a$clase)


# Analisis Exploratorio - Dataset a_big.csv -------------------------------
dim(a_big)
summary(a_big)
plot(a_big[,-c(3,4)], main = "a_big.csv", col = a_big$clase, main = "a_big.csv")


# Analisis Exploratorio - Dataset good_luck.csv ---------------------------
dim(good_luck)
summary(good_luck)
pca = PCA(good_luck)
pca$eigen
readkey()
pairs(good_luck[,-c(11,12)], col=good_luck$clase)


# Analisis Exploratorio - Dataset guess.csv -------------------------------
dim(guess)
summary(guess)
plot(guess, main = "guess.csv")


# Analisis Exploratorio - Dataset h.csv -----------------------------------
dim(h)
summary(h)
plot3d(h$V1, h$V2, h$V3, col=h$clase, type="s")


# Analisis exploratorio  - Dataset help.csv -------------------------------
dim(help)
summary(help)
(help$V1, help$V2, help$V3, col=help$clase, type="s")


# Analisis Exploratorio - Dataset moon.csv --------------------------------
dim(moon)
summary(moon)
plot(moon[,-c(3,4)], col = moon$clase, main="moon.csv")


# Analisis Exploratorio - Dataset s.csv -----------------------------------
dim(s)
summary(s)
plot3d(s$V1, s$V2, s$V3, col=s$clase, type="s")


# Definiendo un numero de clusters en guess.csv ---------------------------
summary(guess)
ss = 1:20
for( i in 1:20){
  modelo = kmeans(x = guess, centers = i, iter.max = 20)
  ss[[i]] = modelo$tot.withinss
}
plot(1:20, ss, xlab = "Clusters", ylab = "Distancia intra clusters total",type = "b")
kGuess = 5


# Resolucion del problema de las clases del dataset help.csv -------------
help$clase2 = 0
help[ 1:1000, ]$clase2 = 1
help[ 1001:2000, ]$clase2 = 2
help[ 2001:3000, ]$clase2 = 3

####### GENERACION DE MODELOS ###########

# Modelo - Dataset a.csv --------------------------------------------------
a.model = kMeans(a[,c(1,2)], centers = 3, max.iter = 50 )
plot(a$V1, a$V2, col=a.model[[2]])
points(a.model[[1]], col=4, pch=19)


# Modelo - Dataset a_big.csv ----------------------------------------------
a_big.model = kMeans(a_big[,c(1,2)], centers=3, max.iter = 50, centros = a.model[[1]])
plot(a_big$V1, a_big$V2, col=a_big.model[[2]])
points(a_big.model[[1]], col=4, pch=19)


# Modelo - Dataset good_luck.csv ------------------------------------------
good_luck.model = kmeans(good_luck[,-c(11,12)], centers=2, iter.max=20)
plot(good_luck[,-c(11,12)], col=good_luck.model$cluster)


# Modelo - Dataset guess.csv ----------------------------------------------
guess.model = kmeans(guess, centers = 5, iter.max=30)
guess$cluster = guess.model$cluster
plot(guess$V1, guess$V2, col=guess$cluster)
points(guess.model$centers, pch=19, col=6, main="Dataset guess.csv")

# Modelo - h.csv ----------------------------------------------------------
h.distance.matrix = dist(as.matrix(h[,c(1,2,3)]))
h.model = hclust(h.distance.matrix, method="complete")
h.corte = cutree(h.model, 11)
scatterplot3d(h$V1, h$V2, h$V3, color=corte, pch=19, main="Dataset h.csv")

# Modelo - s.csv ----------------------------------------------------------
s.distance.matrix = dist(as.matrix(s[,c(1,2,3)]))
s.model = hclust(s.distance.matrix, method="complete")
s.corte = cutree(s.model, 10)
scatterplot3d(s$V1, s$V2, s$V3, color=corte, pch=19, main="Dataset s.csv")

# Modelo - help.csv -------------------------------------------------------
help.distance.matrix = dist(as.matrix(help[,c(1,2,3)]))
help.model = hclust(help.distance.matrix, method="single")
help.corte = cutree(help.model,3)
scatterplot3d(help$V1, help$V2, help$V3, color=corte, pch=19, main="Dataset help.csv")


# Modelo - moon.csv -------------------------------------------------------
moon.distance.matrix = dist(as.matrix(moon[,c(1,2)]))
moon.model = hclust(moon.distance.matrix, method="single")
moon.corte = cutree(moon.model, 2)
plot(moon$V1, moon$V2, col=corte, main="Dataset moon.csv")

####### EVALUACION DE MODELOS ###########


# Evaluacion - K-Medias a.csv ---------------------------------------------
a.conf = table(a$clase, a.model[[2]], dnn=c("Clase", "Cluster"))
a.conf = fix.conf.table(a.conf)
a.conf
eval.conf.matrix(a.conf)


# Evaluacion - K-Medias a_big.csv -----------------------------------------
a_big.conf = table(a_big$clase, a_big.model[[2]], dnn=c("Clase", "Cluster"))
a_big.conf = fix.conf.table(a_big.conf)
a_big.conf
eval.conf.matrix(a_big.conf)


# Evaluacion - K-Medias good_luck.csv -------------------------------------
good_luck.conf = table(good_luck$clase, good_luck.model$cluster, dnn=c("Clase", "Cluster"))
good_luck.conf = fix.conf.table(good_luck.conf)
good_luck.conf
eval.conf.matrix(good_luck.conf)


# Evaluacion - Clusterizacion Jerarquica h.csv ----------------------------
h.conf = table(h$clase, h.corte, dnn=c("Clase", "Cluster"))
h.conf = fix.conf.table(h.conf)
h.conf
eval.conf.matrix(h.conf)


# Evaluacion - Clusterizacion Jerarquica s.csv ----------------------------
s.conf = table(s$clase, s.corte, dnn=c("Clase", "Cluster"))
s.conf = fix.conf.table(s.conf)
s.conf
eval.conf.matrix(s.conf)


# Evaluacion - Clusterizacion Jerarquica help.csv -------------------------
help.conf = table(help$clase2, help.corte, dnn=c("Clase", "Cluster"))
help.conf = fix.conf.table(help.conf)
help.conf
eval.conf.matrix(help.conf)


# Evaluacion - Clusterizacion Jerarquica moon.csv -------------------------
moon.conf = table(moon$clase, moon.corte, dnn=c("Clase", "Cluster"))
moon.conf = fix.conf.table(moon.conf)
moon.conf
eval.conf.matrix(moon.conf)


