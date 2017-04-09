#installing packages
install.packages("xlsx")
install.packages("jsonlite")
install.packages("rJava")
install.packages("curl")


#sources and librarys
library(xlsx)
library(curl)
source("google_api.R")


#Reading data
data <- read.xlsx("hogares.xlsx", sheetIndex = 1, header = T, startRow = 1, endRow = 104)

#Irrelevant Features
data$Foto <- NULL
data$Piso <- NULL

#Adding Services
data$Condominio <- 0
data$Servicio.Basura <- 0
data$Agua <- 0
data$Internet <- 0
data$Calefaccion <- 0

#Miscs
data$Entrada <- 0
data$Habitacion <- 0
data$Cocina <- 0
data$Bano <- 0
data$Terraza <- 0
data$Sala <- 0

#Para hombres, mujeres o unisex (1,2 o 3)
data$Disponibilidad <- 0

#Tipo de habitacion (Monolocale, Singola, Doppia, Intero apartamento, Mini-Apartamento, Posto Letto)
data$Tipo.Habitacion <- 0

#Duracion entre origen y destino, segun google maps, en minutos
data$Tiempo <- 0


#Fixing. The API Returned status!=OK for these values before
data[11,]$Dirección = "Galliate"
data[33,]$Dirección = "Via San Roberto Bellarmino"
data[61,]$Dirección = "Via di Monte Verde"

#Using Google API for distance
APIKey <- "AIzaSyCCcqG81xzm1jWcgRs095rx7s913eOiQvc"
data$Dirección <- gsub("[\n\r]", " ", data$Dirección)
data$Dirección <- as.character(data$Dirección)
destino <- "Piazzale Aldo Moro"
for(i in seq(1:nrow(data)))
{
  origen <- as.character(data$Dirección[i])
  api_url <- get_url(origen, destino, APIKey)
  datos <- get_data(api_url)
  rAPI <- parse_data(datos)
  if(rAPI$status == "OK")
  {
    aux <- strsplit(as.character(rAPI$duration$text), " ")
    aux <- paste(aux[[1]], collapse = "")
    aux <- strsplit(as.character(aux), "min")
    aux <- strsplit(as.character(aux), "h")
    if (length(aux[[1]]) == 1)
    {
      data$Tiempo[i] = aux[[1]][1]
    }
    else 
    {
      data$Tiempo[i] = (as.integer(aux[[1]][1])*60) + as.integer(aux[[1]][2])
    }
  }
}
data$Tiempo <- as.numeric(data$Tiempo)

#Disponibilidad Hombres: 1 Mujeres: 2 Ambos: 3
ambos <- grep("ragazzi/ragazze", as.character(data$Notas))
aux <- grep('ragazze/ragazzi', as.character(data$Notas))
ambos <- union(ambos, aux)
hombres <- grep('ragazzi', as.character(data$Notas))
hombres <- setdiff(hombres, ambos)
aux = union(ambos, hombres)
data[hombres,]$Disponibilidad <- 1
data[-aux,]$Disponibilidad <- 2
data[ambos,]$Disponibilidad <- 3

#Tipo de Cocina Cucina:1 Cucina/living:2 Angolo Cottura:3 Cucina abitabile:4
cAbitabile <- grep('cucina abitabile', data$Descripción, ignore.case = T)
aCottura <- grep('angolo cottura', data$Descripción, ignore.case = T)
cLiving <- grep('cucina/living', data$Descripción, ignore.case = T)
aux <- union (cAbitabile, aCottura)
aux <- union(aux, cLiving)
data[-aux, ]$Cocina <- 1
data[cLiving, ]$Cocina <- 2
data[aCottura, ]$Cocina <- 3
data[cAbitabile, ]$Cocina <- 4

#Baños
banos1 <- grep('bagno', data$Descripción, ignore.case = T)
banos2 <- grep('2 bagni', data$Descripción, ignore.case = T)
banos3 <- grep('3 bagni', data$Descripción, ignore.case = T)
banos4 <- grep('4 bagni', data$Descripción, ignore.case = T)
data[banos1, ]$Bano <- 1
data[banos2, ]$Bano <- 2
data[banos3, ]$Bano <- 3
data[banos4, ]$Bano <- 4
data[76, ]$Bano <- 2

#Habitaciones
habitaciones5 <- grep('5 camere', data$Descripción, ignore.case = T)
habitaciones4 <- grep('4 camere', data$Descripción, ignore.case = T)
habitaciones3 <- grep('3 camere', data$Descripción, ignore.case = T)
habitaciones2 <- grep('2 camere', data$Descripción, ignore.case = T)
habitaciones1 <- grep('camera', data$Descripción, ignore.case = T)
habitaciones4 <- union(16, habitaciones4)
habitaciones2 <- union(92, habitaciones2)
habitaciones3 <- union(1, habitaciones3)
data[habitaciones1, ]$Habitacion <- 1
data[habitaciones2, ]$Habitacion <- 2
data[habitaciones3, ]$Habitacion <- 3
data[habitaciones4, ]$Habitacion <- 4
data[habitaciones5, ]$Habitacion <- 5
rownames(data[ data$Habitacion == 0, ]) #For fixing rare values

#Terraza Terrazo: 1 Terrazzino: 2 Balcone: 3
terraza1 <- grep('terrazzo', data$Descripción, ignore.case = T)
terraza2 <- grep('balcone', data$Descripción, ignore.case = T)
data[terraza1, ]$Terraza <- 1
data[terraza2, ]$Terraza <- 2


#Sala de estar Soggiorno:1 salone/living:2 salotto:3 salottino:4
sala1 <- grep('soggiorno', data$Descripción, ignore.case = T)
sala2 <- grep('salone', data$Descripción, ignore.case = T)
sala3 <- grep('salotto', data$Descripción, ignore.case = T)
sala4 <- grep('salottino', data$Descripción, ignore.case = T)
aux <- grep('living', data$Descripción, ignore.case = T)
sala2 <- union(sala2, aux)
data[sala1, ]$Sala <- 1
data[sala2, ]$Sala <- 2
data[sala3, ]$Sala <- 3
data[sala4, ]$Sala <- 4

#Entrada
entrada <- grep('ingresso', data$Descripción, ignore.case = T)
data[entrada, ]$Entrada <- 1

#Adding Rows. One for each available room (only if there are more than 1)
aux <- data$Precio.Mensual
data$Num.Habitaciones <- gsub(pattern = '([0-9]+).*$', replacement = '\\1', x = data$Habitaciones.Disponibles)
data$Num.Habitaciones <- gsub(pattern = '(intero appartamento)|(mini appartamento)|monolocale',
                             replacement = '1', x = data$Num.Habitaciones, ignore.case = T)
data$Num.Habitaciones <- as.numeric(data$Num.Habitaciones)
data <- data[rep(seq_len(nrow(data)), data$Num.Habitaciones),]
aux <- na.omit(as.numeric(unlist(strsplit(unlist(as.character(data$Precio.Mensual)),"[^0-9]+"))))
aux[147]
length(aux)
data$Precio.Habitacion <- 0
i <- 1
while (i != (nrow(data)+1)) {
  array <- na.omit(as.numeric(unlist(strsplit(unlist(as.character(data$Precio.Mensual[i])), 
                                              "[^0-9]+"))))
  if (data$Num.Habitaciones[i] == 1){
    data$Precio.Habitacion[i] <- array[1]
    i <- i + 1
  }else{
    for (j in 1:length(array)) {
      data$Precio.Habitacion[i + (j-1)] <- array[j]
    }
    i <- i + as.numeric(data$Num.Habitaciones[i])
  }
}
rownames(data) <- 1:nrow(data)
aux <- rownames(data[ data$Precio.Habitacion == 0, ])
aux <- as.numeric(aux)
for( i in aux ){
  data[i,]$Precio.Habitacion <- data[i-1, ]$Precio.Habitacion
}

#Tipo de Habitacion singola:1 doppia:2 posto letto:3 intero appartamento:4 monolocale:5
tHab1 <- grep('singola|singole', data$Habitaciones.Disponibles, ignore.case = T)
tHab2 <- grep('doppia|doppie', data$Habitaciones.Disponibles, ignore.case = T)
tHab2 <- setdiff(tHab2, tHab1)
tHab3 <- grep('posto letto', data$Habitaciones.Disponibles, ignore.case = T)
tHab3 <- setdiff(tHab3, tHab1)
tHab3 <- setdiff(tHab3, tHab2)
tHab4 <- grep('intero appartamento', data$Habitaciones.Disponibles, ignore.case = T)
tHab5 <- grep('monolocale', data$Habitaciones.Disponibles, ignore.case = T)
tHab6 <- grep('mini appartamento', data$Habitaciones.Disponibles, ignore.case = T)
data[tHab1, ]$Tipo.Habitacion <- 1
data[tHab2, ]$Tipo.Habitacion <- 2
data[tHab3, ]$Tipo.Habitacion <- 3
data[tHab4, ]$Tipo.Habitacion <- 4
data[tHab5, ]$Tipo.Habitacion <- 5
data[tHab6, ]$Tipo.Habitacion <- 6

#Servicios
tIncluido <- grep('tutto incluso', data$Precio.Mensual, ignore.case = T)
tIncluido <- union(tIncluido, 78)
noIncluido <- grep('spese escluse', data$Precio.Mensual, ignore.case = T)
complemento <- setdiff(1:103, union(tIncluido, noIncluido))
calefaccion <- grep('riscaldamento', data$Precio.Mensual, ignore.case = T)
agua <- grep('acqua', data$Precio.Mensual, ignore.case = T)
internet <- grep('internet', data$Precio.Mensual, ignore.case = T)
condominio <- grep('condominio', data$Precio.Mensual, ignore.case = T)
sBasura <- grep('rifiuti', data$Precio.Mensual, ignore.case = T)
data[calefaccion, ]$Calefaccion <- 1
data[agua, ]$Agua <- 1
data[internet, ]$Internet <- 1
data[condominio, ]$Condominio <- 1
data[sBasura, ]$Servicio.Basura <- 1
#Todo incluido (el precio incluye todos los servicios)
data[tIncluido, ]$Agua <- 1
data[tIncluido, ]$Internet <- 1
data[tIncluido, ]$Servicio.Basura <- 1
data[tIncluido, ]$Condominio <- 1
data[tIncluido, ]$Calefaccion <- 1
data[48,]$Internet <- 0

#Asignar valor a partir de caracteristicas
vTiempo <- (round(1/data$Tiempo * 500))
valor <- 1:nrow(data)
for( i in 1:nrow(data) ){
  vCondominio <- (data[2,]$Condominio) * 50
  vBasura <- data[i,]$Servicio.Basura * 50
  vAgua <- data[i,]$Agua * 50
  vInternet <- data[i,]$Internet * 75
  vCalefaccion <- data[i,]$Calefaccion * 75
  valor[i] <- vCondominio + vBasura + vAgua + vInternet + vCalefaccion + vTiempo[i]
}
data$Valor <- valor

####Sampling####
set.seed(666)
hombres <- data$Disponibilidad ==1
aux <- data$Disponibilidad == 3
aux <- rownames(data[aux,])
hombres <- rownames(data[hombres,])
hombres <- union(hombres, aux)
mujeres <- data$Disponibilidad ==2
aux <- data$Disponibilidad == 3
aux <- rownames(data[aux,])
mujeres <- union(mujeres, aux)
data_h <- data[hombres,]
data_m <- data[mujeres,]
sample_h <- sample(nrow(data_h), nrow(data_h)*0.8, replace = F)
sample_m <- sample(nrow(data_m), nrow(data_m)*0.8, replace = F)
train_h <- data_h[sample_h,]
test_h <- data_h[-sample_h,]
train_m <- data_m[sample_m,]
test_m <- data_m[-sample_m,]

### Modelo para hombres ####
linearModel_h <- lm(Precio.Habitacion ~ Bano + 
                      Cocina + Terraza + Sala +
                      Valor, data = train_h)
regresion_h1 <- predict(linearModel_h, newdata = test_h, type='response')
table(regresion_h1, test_h$Precio.Habitacion)


### Modelo para mujeres ####
linearModel_m <- lm(Precio.Habitacion ~ Bano + 
                      Cocina + Terraza + Sala +
                      Valor + Distrito, data = data_m)
regresion_m1 <- predict(linearModel_m, newdata=test_m,type='response')
table(regresion_m1, test_m$Precio.Habitacion)

## Escogiendo un Hogar
regresion_h2 <- predict(linearModel_h, newdata = data_h)
regresion_m2 <- predict(linearModel_m, newdata = data_m)
data_h$Regresion <- regresion_h2
data_m$Regresion <- regresion_m2
comp <- cbind.data.frame(regresion = (regresion_h2), precio =(data_h$Precio.Habitacion))
comp$diff <- comp$regresion - comp$precio
comp[order(-comp$diff),]
comp <- cbind.data.frame(regresion = (regresion_m2), precio =(data_m$Precio.Habitacion))
comp$diff <- comp$regresion - comp$precio
comp[order(-comp$diff),]
