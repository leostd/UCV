#install.packages("twitteR")
#install.packages("stringr")
#install.packages("dplyr")

#Cargamos los paquetes necesarios
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(dplyr)
library(devtools)
library(rCharts)
library(FactoMiner)

#Colocamos en variables las credenciales proporcionadas por Twitter
api_key <- "8TduouVcjkjasdflkaWi5YDnS2Z6SZxSnNakjflka"
api_secret <- "1ADFaglhaYszOPvqOohFlBxBnFf2gz3zjDVScqiGuU0JM2lakdhfzhWZvGYhYau2"
access_token <- "221541ASFA064-OmMDGnuFoakjhf6YnqcVWufPfsJIucU8rwR5AC6vYi2xGB"
access_token_secret <- "f3bXhy1r1ZHfw7wahYFKnUlkahgflADFA3Rgas5GiIQzZ78UaXeYcqfajsp"
#Autenticacion 
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Obtenemos la informacion del usuario a estudiar
user<-getUser("LeoSantella")
userFriends <- user$getFriends()
userFollowers <- user$getFollowers()

#Asignamos los seguidores a un data frame y colocamos el numero diferenciador
Followers <- twListToDF(userFollowers)
Followers$flag <- 2

#Asignamos los amigos(friends, personas seguidas)
#a un data frame y colocamos el numero diferenciador
Friends<-twListToDF(userFriends)
Friends$flag<- 1
Neighbors<-union(Followers,Friends)

#Eliminacion de atributos poco relevantes para el estudio de los datos
Neighbors$description<-NULL
Neighbors$name<-NULL
Neighbors$screenName<-NULL
Neighbors$location<-NULL
Neighbors$created<-NULL
Neighbors$url<-NULL
Neighbors$protected<-NULL
Neighbors$lang<-NULL
Neighbors$id<-NULL
Neighbors$followRequestSent<-NULL
Neighbors$profileImageUrl<-NULL
Neighbors$verified<-NULL

#Transformacion de los datos a un rango numerico mas util
#Aplicando Logaritmo neperiano a los datos relevantes
Neighbors[Neighbors=="0"]<-1
Neighbors$logFollowersCount <-log(Neighbors$followersCount)
Neighbors$logFriendsCount <-log(Neighbors$friendsCount)
Neighbors$logFavoritesCount<-log(Neighbors$favoritesCount)
Neighbors$logListedCount<-log(Neighbors$listedCount)
Neighbors$logStatusesCount<-log(Neighbors$statusesCount)
data<-Neighbors[7:11]
summary(data)
stats<-prcomp(data)
biplot(stats)
stats
correlation<-cor(data)
correlation
Neighbors$logFavoritesCount<-NULL
Neighbors$logListedCount<-NULL
Neighbors$favoritesCount<-NULL
Neighbors$listedCount<-NULL

#Escritura del archivo de entrada
write.csv(Neighbors, file = "CI1_CI2_LeoSantella.csv")

#Codo de Jambu
Kobject <- data.frame(Neighbors$logStatusesCount,Neighbors$logFollowersCount)
mydata<-Kobject
summary(mydata)
boxplot(mydata)
wss <- rep(0,30)
for (i in 1:30) wss[i] <- (kmeans(mydata,i))$tot.withinss

#Grafico del resultado de la tecnica del codo de Jambu
plot(1:30, wss, type= "b", xlab="Numero de Clusters", ylab= "Inercia Intra-grupos")

#Algoritmo de K-medias
NeighborsMeans <- kmeans(Kobject, centers=5, iter.max=10, nstart=100)

#Colocamos en un atributo el numero del cluster al cual pertenece el registro
Neighbors$cluster <- NeighborsMeans$cluster

#Grafico del resultado del algoritmo de K-medias
p2 <- nPlot(logFollowersCount ~ logStatusesCount, group = 'cluster', data = Neighbors, type = 'scatterChart')
p2$xAxis(axisLabel = 'Statuses Count')
p2$yAxis(axisLabel = 'Followers Count')
p2$chart(tooltipContent = "#! function(key, x, y, e){
return e.point.screenName + ' Followers: ' + e.point.followersCount +' Statuses: ' + e.point.statusesCount
} !#")
p2
