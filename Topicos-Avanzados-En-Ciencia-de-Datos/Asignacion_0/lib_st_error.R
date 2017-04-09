# Error relativo
ER <- function(Pron, Real) {
  return(sum(abs(Pron - Real))/abs(sum(Real)))
}

# mean squared error (MSE)
ECM <- function(Pred, Real) {
  N <- length(Real)
  ss <- sum((Real - Pred)^2)
  return((1/N) * ss)
}

# Raiz cuadrada del error cuadrativo medio
RECM <- function(Pred, Real) {
  return(sqrt(ECM(Pred, Real)))
}

PFA <- function(Pron, Real) {
  Total <- 0
  N <- length(Pron)
  for (i in 1:N) {
    if (Pron[i] >= Real[i]) 
      Total <- Total + 1
  }
  return(Total/N)
}

PTFA <- function(Pron, Real) {
  Total <- 0
  SReal <- 0
  N <- length(Pron)
  for (i in 1:N) {
    if (Pron[i] >= Real[i]) {
      Total <- Total + (Pron[i] - Real[i])
      SReal <- SReal + abs(Real[i])
    }
  }
  return(Total/SReal)
}