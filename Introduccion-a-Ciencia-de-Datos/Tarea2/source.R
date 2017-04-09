library(devtools)
library(Rfacebook)
library(tm)
library(slam)
library(ggplot2)
library(wordcloud)
library(fastcluster)


fb_oauth<-"CAACEdEose0cBAL1pzdsfafareqwtq5LEdfgsrtSbKXrffZrtrasqrtqtrBZBwWgmQpbKQCQCpJcRp3aaj55bc37IGpTJIuYLE6urdRZAZB2nrMzpj8RWp5kBshkXoBYG2302wT7D5gNM8VnKjdd8pxPpDHby2r1By6nXcVlGKAaXd9YarP7uBIVahjQmZC89iYP6y1Eldd0V3MoRyEkeGPJNy5Is9dyfTvIazutP6041gjzmAJ9dPlLyn8OkZD"
feeds <- getNewsfeed(fb_oauth, n = 1000)
write.csv(feeds, file = "21014872_22022441_23194702_LeonardoSantella_posts.csv")
#------Fin Autenticacion y descargar de 1000 news feeds--------

#posts <- read.csv("21014872_22022441_23194702_LeonardoSantella_posts.csv")

#Eliminamos los posts NA
noNA<-na.omit(posts)
library(tm)
myCorpus <- Corpus(VectorSource(noNA$x))
#myCorpus <- tm_map(myCorpus,content_transformer(function(x) iconv(enc2utf8(x), sub='byte')))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
mycorpus <- tm_map(myCorpus,content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus,content_transformer(stripWhitespace))
myCorpus <- tm_map(myCorpus,content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myStopwords <- c(stopwords(kind="es"), stopwords(kind='en'), 'mierda')
myCorpus <- tm_map(myCorpus, content_transformer(removeWords), myStopwords)
myCorpsCopy <- myCorpus
myCorpus <- tm_map(myCorpus, content_transformer(stemDocument), language=("spanish"))
myTdm <- TermDocumentMatrix(myCorpus,control=list(wordLengths=c(1,Inf)))

#Colocamos los datos de una manera que se pueda manipular su frecuencia y su visualizacion sea comoda
data <- as.data.frame(unlist(findFreqTerms(myTdm, lowfreq=1)))
termFrequency <- rowSums(as.matrix(myTdm))
data$frquency <- termFrequency

#Grafico de frecuencia
barplot(termFrequency, las=2)

#En este punto removemos algunos elementos de la Term-Document Matrix para poder trabajar de manera eficiente
myTdm2 <- removeSparseTerms(myTdm, sparse=0.98)
m2 <- as.matrix(myTdm2)
distMatrix <- dist(scale(m2))
fit<-(hclust(distMatrix, method="ward.D"))
groups <- cutree(fit, k=2)

data <- as.data.frame(unlist(findFreqTerms(myTdm2, lowfreq=1)))
termFrequency <- rowSums(as.matrix(myTdm2))
data$frquency <- termFrequency
data$group <- groups
write.csv(data, file ="21014872_22022441_23194702_LeonardoSantella_posts_facebook_words.csv")