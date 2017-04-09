## En este archivos se definiran las funciones a utilizar, 
## incluyendo las requeridas en el enunciado

MultMV_1.mr.modif <- function( M, V) {
  
  d <- values(from.dfs(V))
  f <- function(x){return(x[3]*d[ d$X == x[2],2])}
  
  map <- function(.,m) {
    i <- m[1]
    m <- as.matrix(m)
    valor <- apply(m,1,f)
    valor <- as.data.frame(as.numeric(as.character(valor)))
    return( keyval(i, valor) )
  }
  
  reduce <- function(i, xi) { 
    keyval(i, sum(xi))
  }
  
  calc <- mapreduce(input=M, 
                    #output=output, 
                    #input.format="text", 
                    map=map, 
                    reduce=reduce,
                    verbose = FALSE)
  calc
}

mat.vec.division = function(M,V,n,k)
{
  #Asumimos que el vector y la matriz son cuadradas, en caso contrario se emitira
  #un error y se detendra la ejecucion.
  if( nrow(M) != n*n )
    stop('La matriz no es cuadrada.')
  if( nrow(V) != n )
    stop('El vector no tiene el numero adecuado de filas.')
  if( n %% k != 0 )
    stop('El k seleccionado no genera submatrices cuadradas.')
  block.size = n/k
  vec.parts = list()
  mat.parts = list()
  l = 1
  for( i in 1:k){
    lim.inf.f = (((i-1)*block.size)+1)
    lim.sup.f = i*block.size
    vec.parts[[i]] = to.dfs(V[ V$X>=lim.inf.f & V$X<=lim.sup.f, ])
    mat.parts[[i]] = to.dfs(M[ M$j<=lim.sup.f & M$j>=lim.inf.f, ])
  }
  result = list()
  result[[1]] = mat.parts
  result[[2]] = vec.parts
  return (result)
}

## Funcion que multiplica una matriz y un vector cuando ambos no
## caben en la memoria y se encuentran en HDFS
MultMV_2.mr = function(M, V)
{
  #En este caso, M y V son listas que referencian a las submatrices(M) y a los subvectores(V)
  k = length(V)
  mids = list()
  for( i in 1:k ){
    mids[[i]] = MultMV_1.mr.modif(M[[i]], V[[i]])
  }
  result = gather(mids, reduce=function(k,v){keyval(k, sum(v))})
  from.dfs(result)
}

MultMM_1.mr = function(A, B,r,c){
  map.A=function(.,X){
    Y = X[rep(seq_len(nrow(X)), each=c),]
    Y$k = rep(seq_len(c), nrow(X))
    Y$m = 'A'
    keyval( Y[,c('i','k')],Y[,c('j','mij', 'm')] )
  }
  map.B=function(.,X){
    Y = X[rep(seq_len(nrow(X)), each=r),]
    k = rep(seq_len(r), nrow(X))
    aux = Y$i
    Y$i = k
    aux2 = Y$j
    Y$k = aux2
    Y$j = aux
    Y$m = 'B'
    keyval(Y[,c('i', 'k')],Y[,c('j','mij', 'm')])
  }
  reduce.red = function(k,v){
    listA = v[v$m == 'A',]
    listB = v[v$m == 'B',]
    listA = listA[order(listA['j']), 'mij']
    listB = listB[order(listB['j']), 'mij']
    mij = sum(listA * listB)
    data = data.frame(k$i, k$k, mij)
    colnames(data) = c('i', 'j', 'mij')
    keyval(1, data)
  }
  A.mr = mapreduce(input=A, map=map.A)
  B.mr = mapreduce(input=B, map=map.B)
  list(A.mr, B.mr)
  result = gather(list(A.mr,B.mr), reduce=reduce.red)
  values(from.dfs(result))
}



