################################################################
# Tarea:    Sets de datos para series de tiempo
# Dataset:  Varios
# Autor:    Jose R Sosa
# Descripción: Carga de varias estructuras de series de datos.
# Se prueba como en algunos casos la regresion lineal o no lineal
# es insuficiente para pronósticar
################################################################


################################################################
# LakeHuron (promedios anuales del nivel del agua en el lago)
################################################################
#install.packages("fracdiff")
#install.packages("tseries")
library(tseries)
library(forecast)

data(LakeHuron)
seriet <- LakeHuron
head(seriet)
class(seriet) # vemos que ya es de tipo "ts"
plot(seriet, type = "o")



################################################################
# Beer (venta de cerveza en Australia)
################################################################
beer <- read.csv("http://josersosa.blogsite.org/~jsosa/data/beer.txt", 
                 header = FALSE, dec = ".", sep = ";")
beer
dim(beer)
lbeer <- log(beer)
t <- seq(1956, 1995.2, length = length(t(lbeer)))
names(lbeer) <- c("lbeer")
lbeer$t <- t
plot(lbeer$t, lbeer$lbeer, type = "l")
plot(lbeer$t, lbeer$lbeer, type = "o")

## Convertimos la data a serie de tiempo
beer <- ts(beer[, 1], start = 1956, freq = 12)
seriet <- beer
class(seriet) # vemos que ya es de tipo "ts"
plot(seriet, type = "l")


################################################################
# Cajero automático 102 (Retiro de efectivo diario)
################################################################
caj102 <- read.csv("http://josersosa.blogsite.org/~jsosa/data/cajero102.txt", 
                 header = FALSE, dec = ".", sep = ";")
head(caj102)
dim(caj102)
caj102 <- ts(caj102, start = c(2008, 1), freq = 365)
plot(caj102, type = "o")
t <- seq(2008, 2012.2, length = length(caj102))



################################################################
# Cajero automático 104 (Retiro de efectivo diario)
################################################################
caj104 <- read.csv("http://josersosa.blogsite.org/~jsosa/data/cajero104.txt", 
                   header = FALSE, dec = ".", sep = ";")
head(caj104)
dim(caj104)
caj104 <- ts(caj104[, 1], start = c(2008, 1), freq = 365)
caj104
plot(caj104, type = "o")

seriet <- caj104
class(seriet) # vemos que ya es de tipo "ts"
plot(seriet, type = "o")



################################################################
# WWWusage (número de visitas aun página web)
################################################################
data(WWWusage)
seriet <- WWWusage
seriet
class(seriet) # vemos que ya es de tipo "ts"
plot(seriet, type = "o")



################################################################
# Pasajeros de aerolineas
################################################################
data(AirPassengers)
seriet <- AirPassengers
seriet
summary(seriet)
class(seriet) # vemos que ya es de tipo "ts"
plot(seriet, type = "l")



################################################################
# Serie Caótica
################################################################
# Funcion logística
logistic <- function(r,x) r*x*(1-x)

# Serie generada por de la función logística para un r y cond inicial dados: Xn-1, Xn, ...
logistic_serie <- function(r,xini,npoints)
{ 
  niter <- 1:npoints
  nserie <- 1:npoints
  x <- xini
  for (i in 1:npoints) {
    x <- logistic(r,x)
    nserie[i] <- x
  }
  nserie
}

seriet <- logistic_serie(4, 0.66, 200)
seriet <- ts(seriet, start = c(2000, 1), freq = 12)
seriet
summary(seriet)
class(seriet) # vemos que ya es de tipo "ts"
plot(seriet, type = "l")


