itemTransform = function(x)
{
    clases = c("deportes", "politica", "variedades",
               "internacional","nacionales", "sucesos", 
               "comunidad", "negocios", "opinion" )
    articulo = x %% 9
    clase = x %/% 9 + 1
    if (articulo == 0)
    {
      articulo = 9
      clase = clase - 1
    }

    return (c(paste(clases[clase],"/","articulo",articulo,sep=""), paste(clases[clase], sep = "")))
}

convertItems = function(x)
{
  items = as.integer(x)
  newItems = itemTransform(items[1])
  newItem = newItems[1]
  newCategorie = newItems[2]
  if( length(items) == 1 )
  {
    return (list(newItem, newCategorie))
  }
  
  for ( i in 2:length(items))
  {
    aux = itemTransform(items[i])
    newItem = paste(newItem, ",",aux[1], sep="")
    newCategorie = paste(newCategorie, ",", aux[2], sep="")
  }
  return (list(newItem, newCategorie))
}


generateTransactions = function(x,y)
{
  y[x] = 1
  y
}

generate_ROC = function(scores, real, target)
{
  df = data.frame(scores, real)
  df = df[with(df,order(-scores)),]
  FP = 0
  TP = 0
  x = numeric()
  y = numeric()
  fprev = -Inf
  i = 1
  P = length(real[real==target])
  N = length(real[real!=target])
  while( i <= nrow(df) )
  {
    if( df$scores[i] != fprev )
    {
      x[i] = FP/N
      y[i] = TP/P
      fprev = df$scores[i]
    }
    if( df$real[i] == target )
    {
      TP = TP + 1
    }
    else
    {
      FP = FP + 1
    }
    i = i +1
  }
  x[i] = FP/N
  y[i] = TP/P
  x = x[!is.na(x)]
  y = y[!is.na(y)]
  plot(x,y,type = "b", main = "ROC Curve", xlab = "FP-Rate", ylab = "TP-Rate", xlim=c(0,1), ylim=c(0,1) )
  lines(seq(from=0, to=1, by=0.01), seq(from=0, to=1, by=0.01))
}