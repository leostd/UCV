#Funcion para asignar clases a traves de intervalos
asignarClase = function(x,c){
  ncortes = length(x) - 1
  result = c
  for(i in 1:ncortes){
    result[c > x[i] & c < x[i+1]]= i
  }
  result
}

#Funcion para esperar que se presione una tecla
readkey = function(){
  cat ("Press [enter] to continue")
  line <- readline()
}

#Definicion del algoritmo K-Medias con la distancia euclidiana como medida
kMeans = function(data, centers, max.iter, centros = NULL){
  #inicializamos los datos a usar
  x = data
  
  #inicializacion de un contador, para terminar la ejecucion si no hay cambios relevantes
  first = rep(0,centers)
  
  #si no se proveen los centroides, elegimos unos al azar
  if ( is.null(centros) ){
    centroids = data[1:centers,]
    for( i in 1:3 ){
      centroids[i,] = runif(2, min = min(x$V1), max = max(x$V1))
    }
  }
  else{
    centroids = centros
  }
  
  #Funcion para calcular la distancia euclidiana
  distancia = function(x1, y1, x2, y2){
    dist = sqrt( (x2-x1)^2 + (y2-y1)^2 )
    dist
  }
  
  #Funcion para asignar los clusters
  clust = function(x){
    which( x == min(x), arr.ind=T )
  }
  
  #iteramos hasta el maximo de iteraciones
  for(i in 1:max.iter){
    
    #Inicializamos la matriz de distancias (distancia entre cada centroide y los puntos)
    distancias = matrix(nrow = nrow(x), ncol=centers)
    
    #Calculamos las distancias
    for( i in 1:centers ){
      distancias[,i] = distancia(centroids[i,1], centroids[i,2], x[,1], x[,2])
    }
    
    #Definimos un cluster para cada punto
    clusters = apply(distancias, 1, clust)
    
    #Bandera para la terminacion temprana del algoritmo
    flag = T
    
    #Calculamos la cantidad de elementos que pertenecen a cada cluster
    for(i in 1:centers)
    {
      
      #Si alguna cantidad cambia, actualizamos las cantidades y colocamos en F la bandera
      if( first[[i]] != length(clusters[clusters==i])){
        for(i in 1:centers)
          first[[i]] = length(clusters[clusters==i])
        flag = F
        break
      }
    }
    
    #Si la bandera es T, quiere decir que no hubo cambios relevantes y por lo tanto, terminamos el ciclo
    if(flag == T)
      break
    
    #Recalculamos los centroides
    for(i in 1:centers)
    {
      for(j in 1:ncol(centroids)){
        centroids[i,j] = mean(x[clusters==i,j])
      }
    }
  }
  
  #Retornamos una lista con los centroides y un vector que contiene a que cluster pertenece cada punto
  list(centroids, clusters)
}

#Funcion que intercambia dos columnas en un data.frame o una matriz
swap.columns = function(df,i,j){
  x = df
  aux = x[,i]
  x[,i] = x[,j]
  x[,j] = aux
  x
}

#Funcion que acomoda los resultados generados por el modelo de clusterizacion
fix.conf.table = function(df){
  x = df
  filas = nrow(x)
  columnas = ncol(x)
  mark = rep(F, columnas)
  for(i in 1:filas){
    fila.ordenada = sort(x[i,], decreasing=T)
    indices.ordenados = as.integer(names(fila.ordenada))
    for(j in indices.ordenados)
    {
      if(j == i)
      {
        mark[i] = T
        break
      }
      else
      {
        if(!mark[i])
        {
          mark[i] = T
          x = swap.columns(x,i,j)
          break
        }
      }
    }
    if( all(mark) )
      break
  }
  x
}

#Evaluacion de Matriz de confusion
eval.conf.matrix = function(m){
  filas = nrow(m)
  if( filas > 2 )
  {
    tps = 0
    fs = 0
    for( i in 1:filas )
    {
      tps = tps + m[i,i]
      for( j in (1:filas)[-i] )
      {
        fs = fs + m[i,j]
      }
    }
    cat("La exactitud del modelo es: ", tps/(tps+fs),"\n")
    cat("tps: ", tps,"\n")
    cat("fs: ", fs,"\n")
  }
  else if( filas == 2 )
  {
    precision = m[1,1]/(m[1,1]+m[2,1])
    sensibilidad = m[1,1]/(m[1,1]+m[1,2])
    f1 = 2*( (precision * sensibilidad) / (precision + sensibilidad) )
    cat("Precision: ", precision,"\n")
    cat("Sensibilidad: ", sensibilidad,"\n")
    cat("F1 Score: ", f1,"\n")
  }
}

