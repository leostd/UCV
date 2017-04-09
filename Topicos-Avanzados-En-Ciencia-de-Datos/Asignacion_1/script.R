## Comando para establecer el directorio de trabajo
setwd("/home/lsantella/Desktop/TACD/Asignacion_1")

## Inicializo las variables de entorno de hadoop
Sys.setenv("HADOOP_PREFIX"="/home/lsantella/hadoop-2.7.2")
Sys.setenv("HADOOP_CMD"="/home/lsantella/hadoop-2.7.2/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/home/lsantella/hadoop-2.7.2/share/hadoop/tools/lib/hadoop-streaming-2.4.0.jar")

## Cargamos las fuentes y bibliotecas necesarias
## Libreria de manejo de HDFS
library(rhdfs)
hdfs.init()

## Libreria de MapReduce sobre Hadoop
library(rmr2) 
ignore <- rmr.options(backend="local") # Opciones "local" o "hadoop"
library(dplyr)
## Cargo la funcion de multiplicacion de Matriz x Vector, Tipo 1
source("prod_Mv_type1.R")
source('funciones.R')
#Cargamos los datos de prueba. Ya estan en un formato clave-valor
mat10 = read.csv('tblAkv10x10.csv')
mat10ident = read.csv('tblAkv10x10ident.csv')
vec10 = read.csv('tblxkv10.csv')
mat3 = read.csv('tblAkv3x3.csv')
vec3 = read.csv('tblxkv3.csv')

## Dividimos la matriz 10x10 en k bloques
list = mat.vec.division(mat10,vec10,10,2)
## Ahora tenemos 2 listas. Una referencia las k submatrices en HDFS y la otra lista
## los k subvectores.
mat.parts = list[[1]]
vec.parts = list[[2]]
#Procedemos a obtener los vectores que tienen los resultados parciales
result = MultMV_2.mr(mat.parts, vec.parts)
result

#### Multiplicacion Matriz-Matriz ####
A = to.dfs(mat10)
B = to.dfs(mat10ident)
## La funcion recibe como parametros, las referencias de ambas matrices en dfs,
## el numero de filas de la primera matriz y el numero de columnas de la segunda
result = MultMM_1.mr(A, B,10,10)
result
