# var1_to_vma
# Convert VAR(1) to VMA(q) representation
# @export

var1_to_vma <- function(Amat=NULL, q=10){
  # Check if Amat is a matrix
  if (!is.matrix(Amat)) {
    stop("Amat must be a matrix.")
  }
  
  # Check if q is a positive integer
  if (!is.numeric(q) || q <= 0 || q != round(q)) {
    stop("q must be a positive integer.")
  }
  
  # Get the number of variables
  K <- nrow(Amat)
  
  # Initialize the VMA(q) matrix
  VMA <- array(0, dim = c(K, K, q))
  
  # Fill in the VMA(q) matrix
  for (i in 1:q) {
    if (i == 1) {
      VMA[, , i] <- diag(K)
    } else {
      VMA[, , i] <- Amat %*% VMA[, , i - 1]
    }
  }
  
  return(VMA)  
}
