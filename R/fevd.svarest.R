#' @export
"fevd.svarest" <-
function(x, n.ahead=10, ...){
  if(!is(x, "svarest")){
    stop("\nPlease provide an object of class 'svarest', generated by 'SVAR()'.\n")
  }
  n.ahead <- abs(as.integer(n.ahead))
  K <- x$var$K
  p <- x$var$p
  ynames <- colnames(x$var$datamat[, 1 : K])
  msey <- .fecovsvar(x, n.ahead = n.ahead)
  Phi <- Phi(x, nstep = n.ahead)
  mse <- matrix(NA, nrow = n.ahead, ncol = K)
  Omega <- array(0, dim = c(n.ahead, K, K))
  for(i in 1 : n.ahead){
    mse[i, ] <- diag(msey[, , i])
    temp <- matrix(0, K, K)
    for(j in 1 : i){
      temp <- temp + Phi[ , , j]^2
    }
    temp <- temp / mse[i, ]
    for(j in 1 : K){
      Omega[i, ,j] <- temp[j, ]
    }
  }
  result <- list()
  for(i in 1 : K){
    result[[i]] <- matrix(Omega[, , i], nrow = n.ahead, ncol = K)
    colnames(result[[i]]) <- ynames
  }
  names(result) <- ynames
  class(result) <- "varfevd"
  return(result)
}
