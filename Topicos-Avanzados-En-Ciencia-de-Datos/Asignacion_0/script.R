#Asignacion del directorio de trabajo
setwd("./Asignacion_0")

#Carga de las librerias 
library(rstudio)
library(tseries)
library(forecast)

#Cargamos el archivo con las funciones para el calculo de los errores
source("lib_st_error.R")

#Carga de datos
cajero1 <- read.table("cajero102.txt", 
                          sep="\n", 
                          col.names=c("Efectivo Diario"), 
                          fill=FALSE, 
                          strip.white=TRUE)

cajero2 <- read.table("cajero105.txt", 
                      sep="\n", 
                      col.names=c("Efectivo Diario"), 
                      fill=FALSE, 
                      strip.white=TRUE)

#Conversion de los data.frame a ts (time-series)
cajero1 <- ts(cajero1, start=c(2010,1), frequency=365)
cajero2 <- ts(cajero2, start=c(2010,1), frequency=365)
summary(cajero1)
summary(cajero2)

#Grafico de las series de tiempo
layout(1:2)
cajero1 <- cajero1/1000
cajero2 <- cajero2/1000
plot(cajero1, ylab="Efectivo Diario Cajero1")
plot(cajero2, ylab="Efectivo Diario Cajero2")

layout(1:1)

#Descomposicion de la serie de tiempo a traves de un suavizado
#Solo se obtuvo la tendencia, ya que la estacionalidad no fue posible
cajero1.filtro1 <- filter(cajero1, filter=rep(1/20, 20))
cajero1.filtro2 <- filter(cajero1, filter=rep(1/40, 40))
cajero1.filtro3 <- filter(cajero1, filter=rep(1/70, 70))
cajero1.lm <- lm(cajero1~time(cajero1))
plot(cajero1)
lines(cajero1.filtro1, col="red")
lines(cajero1.filtro2, col="blue")
lines(cajero1.filtro3, col="green")
abline(cajero1.lm)
#Residuales con variacion estacional
cajero1.residuals <- cajero1 - cajero1.filtro2
plot(cajero1.residuals)

#Correlacion Simple y parcial
acf(cajero1)
pacf(cajero1)

#Primeras diferencias del cajero1
adf.test(cajero1.diffs2)
cajero1.diffs1 = diff(cajero1)
plot(cajero1.diffs1)
cajero1.diffs2 = diff(cajero1.diffs1)
plot(cajero1.diffs2)

#Test de normalidad
hist(cajero1, prob=T, col='grey')
mu.cajero1 = mean(cajero1)
sd.cajero1 = sd(cajero1)
lines(density(cajero1), lwd=5)
x <- seq(1000, 15000, length=273)
y<- dnorm(x, mu.cajero1, sd.cajero1)
lines(x,y,lwd=5,col='red')
shapiro.test(cajero1)
hist(cajero1.diffs1, prob=T, col='blue')
mu.diffs1 = mean(cajero1.diffs1)
sd.diffs1 = sd(cajero1.diffs1)
lines(density(cajero1.diffs1), lwd=5)
x <- seq(-10000,10000, length=273)
y<- dnorm(x, mu.diffs1, sd.diffs1)
lines(x,y,lwd=5,col='red')
shapiro.test(cajero1.diffs1)

#Peridiograma del cajero 1
cajero1.peridiograma <- spec.pgram(cajero2)
plot(cajero1.peridiograma)
cajero1.peridiograma$freq
cajero1.peridiograma$spec
order(cajero1.peridiograma$spec, cajero1.peridiograma$freq, decreasing = TRUE)
max1 <- cajero1.peridiograma$freq[19]
max1
max2 <- cajero1.peridiograma$freq[41]
max2
max3 <- cajero1.peridiograma$freq[82]
max3
abline(v = max1, lty = "dotted", col = "red")
abline(v = max2, lty = "dotted", col = "blue")
abline(v = max3, lty = "dotted", col = "green")
periodo1 <- 365/max1
periodo1
abline(v = max2, lty = "dotted", col = "blue")
periodo2 <- 365/max2
periodo2
periodo3 <- 365/max3
periodo3


#Descomposicion de la serie de tiempo (Modelo aditivo)
cajero2.add <- decompose(cajero2)
plot(cajero2.add)

#Correlacion Simple y Parcial del cajero2
acf(cajero2)
pacf(cajero2)

#Primeras Diferencias del cajero 2
adf.test(cajero2)
cajero2.diffs1 = diff(cajero2)
plot(cajero2.diffs1)
cajero2.diffs2 = diff(cajero2.diffs1)
plot(cajero2.diffs2)

#Test de normalidad del cajero2
hist(cajero2, prob=T, col='grey')
mu.cajero2 = mean(cajero2)
sd.cajero2 = sd(cajero2)
lines(density(cajero2), lwd=5)
x <- seq(0, 12000, length=length(cajero2))
y<- dnorm(x, mu.cajero2, sd.cajero2)
lines(x,y,lwd=5,col='red')
shapiro.test(cajero2)
hist(cajero2.diffs1, prob=T, col='blue')
mu.diffs1 = mean(cajero2.diffs1)
sd.diffs1 = sd(cajero2.diffs1)
lines(density(cajero2.diffs1), lwd=5)
x <- seq(-10000,10000, length=length(cajero2))
y<- dnorm(x, mu.diffs1, sd.diffs1)
lines(x,y,lwd=5,col='red')
shapiro.test(cajero2.diffs1)


#Peridiograma del Cajero 2
cajero2.peridiograma <- spec.pgram(cajero2)
plot(cajero2.peridiograma)
cajero2.peridiograma$freq
cajero2.peridiograma$spec
order(cajero2.peridiograma$spec, cajero2.peridiograma$freq, decreasing = TRUE)
max1 <- cajero2.peridiograma$freq[71]
max1
max2 <- cajero2.peridiograma$freq[154]
max2
max3 <- cajero2.peridiograma$freq[309]
max3
abline(v = max1, lty = "dotted", col = "red")
abline(v = max2, lty = "dotted", col = "blue")
abline(v = max3, lty = "dotted", col = "green")
periodo1 <- 365/max1
periodo1
abline(v = max2, lty = "dotted", col = "blue")
periodo2 <- 365/max2
periodo2
periodo3 <- 365/max3
periodo3

#### Eleccion de Modelos ####
best.order <- c(0, 0, 0)
best.aic <- 9999999999
for( i in 0:3 ){
  for( j in 0:3 ){
    fit.aic <- AIC(arima(cajero1, order=c(i,0,j), method="ML"))
    if( fit.aic < best.aic )
    {
      best.order = c(i,0,j)
      cajero1.best.arma = arima(cajero1, order=best.order)
      best.aic = fit.aic
    }
    
  }
}

best.order <- c(0, 0, 0)
best.aic <- 99999999999
for( i in 0:3 ){
  for( j in 0:3 ){
    fit.aic <- AIC(arima(cajero2, order=c(i,0,j), method="ML"))
    if( fit.aic < best.aic )
    {
      best.order = c(i,0,j)
      cajero2.best.arma = arima(cajero2, order=best.order, method="ML")
      best.aic = fit.aic
    }
    
  }
}

#### Calculo del Error ####

cajero1.best.arma
serie.sim1 <- simulate(cajero1.best.arma, future=FALSE, seed=666)
layout(1:2)
plot(serie.sim1)
plot(cajero1)
residuals <- ER(serie.sim2, cajero2 )
residuals


cajero2.best.arma
serie.sim2 <- simulate(cajero2.best.arma, future=FALSE, seed=666)
plot(serie.sim2)
plot(cajero2)
residuals <- ER(serie.sim2, cajero2 )
residuals
layout(1:1)

#### Prediccion ####
cajero1.prediccion <- forecast.Arima(cajero1.best.arma, h = 14)
cajero1.prediccion
plot.forecast(cajero1.prediccion)

cajero2.prediccion <- forecast.Arima(cajero2.best.arma, h = 14)
cajero2.prediccion
plot.forecast(cajero2.prediccion)
