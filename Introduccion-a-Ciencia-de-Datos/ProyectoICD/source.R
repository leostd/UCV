library(tm)
setwd("~/Data Science/ProyectoICD")
data <- read.csv("data.csv", sep=";", row.names=1)


#write.csv(data, file="prueba.csv")
#Inicio Limpieza de datos
#Inicio Limpieza de datos

#Eliminamos las filas NA
data <- na.omit(data)

#Eliminamos la redundacia de espacios en blanco
data$post<-stripWhitespace(as.character(data$post))

#Elimina las filas que solo contienen un caracter no significativo
data<- data[!(is.na(data$post) | data$post=="" | data$post==" " | data$post=="\n" | data$post=="\v") | data$post=="\t" | data$post=="\r" | data$post=="\f",]

#Colocamos todas las letras en minuscula
data$post <- tolower(data$post)

#Removemos palabras no significativas para nuestro estudio
myStopwords <- c(stopwords(kind="es"), stopwords(kind='en'), 'mierda', 'verga', 'puta', 'fuck')
data$post <- removeWords(data$post, myStopwords)

#Eliminamos iconos codificados en UTF-8 (Algunos)
data$post <- removeUTFIcons(data$post)

#Eliminamos signos de puntuacion
data$post <- removePunctuation(data$post)

#Eliminaos los URL 
data$post <- removeURL(data$post)

#Eliminamos por 2da vez la redundacia de espacios en blanco
data$post<-stripWhitespace(as.character(data$post))

#Eliminamos por 2da vez las filas que contienen catracteres no significativos
data<- data[!(is.na(data$post) | data$post=="" | data$post==" " | data$post=="\n" | data$post=="\v") | data$post=="\t" | data$post=="\r" | data$post=="\f",]

#Eliminamos varios elementos antes mencionados en la definicion de esta funcion
data$post <- removeCharacters(data$post)

#Eliminamos los numeros
data$post <- removeNumbers(data$post)

#Eliminamos caracteres individuales y espacios en blanco al inicio del post
data$post <- removeSingles(data$post)

#Eliminamos por ultima vez la redundancia de espacios en blanco
data$post<-stripWhitespace(as.character(data$post))

#Eliminamos las filas con caracteres no signuficativos
data<- data[!(is.na(data$post) | data$post=="" | data$post==" " | data$post=="\n" | data$post=="\v") | data$post=="\t" | data$post=="\r" | data$post=="\f",]

############Fin Limpieza de datos################


corpus <- Corpus(VectorSource(data$post))
tdm <- TermDocumentMatrix(corpus,control=list(wordLengths=c(2,Inf)))
Terminos<- (unlist(findFreqTerms(tdm, lowfreq=1)))

allTerms = data.frame( Termino = character(10382), Frecuencia = numeric(10382))
allTerms$Termino <- (unlist(findFreqTerms(tdm, lowfreq=1)))
frecuenciaTotal <- rowSums(as.matrix(tdm))
allTerms$Frecuencia <- frecuenciaTotal

terminosFrecuentes <- allTerms[ allTerms$Frecuencia > 50, ]
rownames(terminosFrecuentes) <- terminosFrecuentes$Termino
terminosFrecuentes$Termino <- NULL
dim(terminosFrecuentes)
distMatrix <- dist(terminosFrecuentes)
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
rect.hclust(fit, k=3)

aportes <- merge(terminosFrecuentes, Usuario1, all.x = TRUE, by = "row.names")
aportes <- merge(aportes, Usuario2, all.x = TRUE, by.x = "Row.names", by.y = "row.names")
aportes <- merge(aportes, Usuario3, all.x = TRUE, by.x = "Row.names", by.y = "row.names")
aportes <- merge(aportes, Usuario4, all.x = TRUE, by.x = "Row.names", by.y = "row.names")
aportes <- merge(aportes, Usuario5, all.x = TRUE, by.x = "Row.names", by.y = "row.names")
aportes[is.na(aportes)]<- 0

#Formaremos 3 subgrupos de palabras frecuentes
grupos <- cutree(fit, k = 3)
terminosFrecuentes$Grupo <- grupos

Grupo1 <- aportes[aportes$Grupo==1,]
Grupo2 <- aportes[aportes$Grupo==2,]
Grupo3 <- aportes[aportes$Grupo==3,]


freqTerms <- findFreqTerms(tdm, lowfreq=50)
matrixClust <- aportes[c("Row.names","Usuario1", "Usuario2", "Usuario3", "Usuario4", "Usuario5")]
matrixClust <- t(matrixClust)
matrixClust <- matrixClust[2:6,]

distMatrix2 <- dist(matrixClust)
fit2 <- hclust(matrixClust, method = "ward.D")
plot(fit2)
rect.hclust(fit2, k=3)

#Observaremos relaciones entre los usuarios
aportes

#Matriz de correlaciones
cor(aportes[,4:8])

#Analisis de componentes principales
componentes <- PCA(aportes[, 4:8])



####################Funciones############
removeCharacters<- function(x){
  x<-gsub("aa+","a", x)
  x<-gsub("bb+]","b", x)
  x<-gsub("(cc)+c+","c", x)
  x<-gsub("dd+","d", x)
  x<-gsub("ee+","e", x)
  x<-gsub("ff+","f", x)
  x<-gsub("gg+","g", x)
  x<-gsub("hh+","h", x)
  x<-gsub("ii+","i", x)
  x<-gsub("jj+","j", x)
  x<-gsub("kk+","k", x)
  x<-gsub("(ll)+l+","l", x)
  x<-gsub("mm+","m", x)
  x<-gsub("nn+","n", x)
  x<-gsub("oo+","o", x)
  x<-gsub("pp+","p", x)
  x<-gsub("qq+","q", x)
  x<-gsub("rr+r+","r", x)
  x<-gsub("ss+","s", x)
  x<-gsub("tt+","t", x)
  x<-gsub("uu+","u", x)
  x<-gsub("vv+","v", x)
  x<-gsub("ww+","w", x)
  x<-gsub("xx+","x", x)
  x<-gsub("yy+","y", x)
  x<-gsub("zz+","z", x)
  x<-gsub("(jaja)+(ja)*", "haha", x)
  x<-gsub("(jeje)+(je)*", "haha", x)
  x<-gsub("(juju)+(ju)*", "haha", x)
  x<-gsub("(jojo)+(jo)*", "haha", x)
  x<-gsub("(hehe)+(he)*", "haha", x)
  x<-gsub("(haha)+(ha)*", "haha", x)
  x<-gsub("[jska]{6}[jska]*","haha",x)
  x<-gsub("[hsdg]{6}[hdgs]*","haha",x)
  x<-gsub("(xd)+","haha", x)
  x<-gsub("(lol)+[lo]*", "haha",x)
  x<-gsub("videos", "video", x)
  x
}

#Elimina iconos con codificacion UTF-8
removeUTFIcons <- function(x){
  gsub("<[[:alnum:]+[:alnum:]+]+>","",x)
}

#Elimina los numeros
removeNumbers <- function(x){
  gsub("[[:digit:]]", "", x)
}

#Elimina los caracteres individuales y los espacios en blanco al inicio del post
removeSingles <- function(x){
  x <- gsub("^.$","",x)
  x <- gsub("^.[[:space:]]$","",x)
  x <- gsub("^[[:space:]]", "", x)
  x
}

#Elimina los URL de los datos
removeURL <- function(x){
  x<-gsub("http[[:alnum:]]*", "", x)
  x<-gsub("www[[:alnum:]]*", "", x)
  x
} 

####################Fin Funciones#################



