#installing packages
install.packages(c("party","dplyr","rpart"))
install.packages("rattle")
install.packages("FactoMineR")
install.packages("RColorBrewer")
install.packages("rpart.plot")
install.packages("RWeka", dependencies = T)
install.packages("class")
install.packages("rJava")

#library loading
library(party)
library(rpart)
library(dplyr)
library(rattle)
library(FactoMineR)
library(RWeka)
library(rJava)
library(class)
#set random seed
set.seed(666)

#loading data
data <- read.csv("./minable.csv")

#data preprocessing
sapply(data,class)
str(data)
data <- data[-1,]
data$fNacimiento <- NULL
data$jReprobadas <- NULL
data$pReside <- NULL
data$dHabitacion <- NULL
data$cDireccion <-NULL
data$oSolicitudes <- NULL
data$aEconomica <- NULL
data$grOdontologicos <- NULL
data$sugerencias <- NULL
PCA <- PCA(data)

#create train and test data sets by sampling
summary(data$mIngreso)
unique(data$mIngreso)
total <- nrow(data)
prop0 <- sum(data$mIngreso == 0) / total  
prop1 <- sum(data$mIngreso == 1) / total  
prop2 <- sum(data$mIngreso == 2) / total  
prop3 <- sum(data$mIngreso == 3) / total  
proportions <- function(x) 
{
  if(x == 0)
    return (prop0)
  if(x == 1)
    return (prop1)
  if(x == 2)
    return (prop2)
  if(x == 3)
    return (prop3)
}
proportions <- apply(as.matrix(data$mIngreso), 1, proportions)
sampling <- sample(nrow(data), nrow(data)*0.8, prob=proportions, replace=F)
train <- data[sampling,]
test <- data[-sampling,]

#### Decision tree ####

#Conditional Inference tree
tree <- ctree(formula = mIngreso~.,
              data = train)
#Plot ctree
plot(tree)

#Prediction ctree
pred.tree <- predict(tree, 
                     test, 
                     type="class")

#Confusion Matrix ctree. Measuring prediction performance
perf.tree <- table(test$mIngreso, 
                   pred.tree,
                   dnn=c("Actual", "Predicted"))
perf.tree

#Recursive Partition tree
tree <- rpart(formula = mIngreso ~ .,
              data = data,
              method = "class",
              control = rpart.control(minsplit = 40,
                                      maxdepth = 4))

#Plot rpart tree
plot(tree)
text(tree, 
     use.n = T)

#Cofusion Matrix rpart tree. Measuring prediction performance
perf.tree = table(test$mIngreso, 
                            predict(tree, newdata = test,type = "class"),
                            dnn = c("Actual", "Predicted"))

#Hit rate
hrate.tree <- (perf.tree[1,1] + perf.tree[2,2] + perf.tree[3,3]) / nrow(test)
hrate.tree <- hrate.tree * 100
hrate.tree

#### K-Nearest Neighbors ####

#Preprocessing
normalize = function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

knn.train <- train
knn.test <- test
knn.class.train <- knn.train$mIngreso
knn.class.test <- knn.test$mIngreso
knn.test$mIngreso <- NULL
knn.train$mIngreso <- NULL
knn.train <- apply(knn.train,2,normalize)
knn.train <- as.data.frame(knn.train)

#Model and Prediction
knn <- knn(train = knn.train,
           test = knn.test,
           cl = knn.class.train,
           k=7)

#Performance. Confusion matrix
perf.knn <- table(knn.class.test, 
                  knn,
                  dnn=c("Actual", "Predicted"))
perf.knn

#Hit rate. K-Nearest Neighbor
hrate.knn <- (perf.knn[1,1] + perf.knn[2,2] + perf.knn[3,3]) / nrow(knn.test)
hrate.knn <- hrate.knn * 100
hrate.knn

#### Classification rules ####

#Preprocessing
crules.train <- train
crules.test <- test
crules.train$mIngreso <- as.factor(crules.train$mIngreso)
crules.test$mIngreso <- as.factor(crules.test$mIngreso)

#Model
crules <- JRip(formula = mIngreso ~ .,
               data = crules.train)

#Performance. Confusion Matrix
perf.crules <- table(crules.test$mIngreso, 
                     predict(crules,
                             newdata=crules.test),
                     dnn=c("Actual", "Predicted"))
perf.crules

#Hit rate. CRules
hrate.crules <- (perf.crules[1,1] + perf.crules[2,2] + perf.crules[3,3]) / nrow(crules.test)
hrate.crules <- hrate.crules * 100
hrate.crules

#Comparacion de modelos
hrate <- rbind(hrate.tree, hrate.knn, hrate.crules)
hrate

