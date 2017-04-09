
# Multiplicacion Matriz por Vector usando mapreduce
# Tipo 1: el caso en que el vector v cabe dentro de la memoria RAM

MultMV_1.mr <- function( M, V) {
  
  d <- values(from.dfs(V))
  f <- function(x){return(x[3]*d[x[2],2])}
  
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
  C = values( from.dfs( calc ) ) 
  C
}


