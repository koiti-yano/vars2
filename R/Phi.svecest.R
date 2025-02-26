#' @export
"Phi.svecest" <-
function(x, nstep = 10, ...){
  if(!is(x, "svecest")){
    stop("\nPlease provide an object of class 'svecest', generated by 'SVEC()'.\n")
  }
  nstep <- abs(as.integer(nstep))
  varlevel <- vec2var(x$var, r = x$r)
  Phi <- Phi(varlevel, nstep = nstep)
  for(i in 1: dim(Phi)[3]){
    Phi[, , i] <- Phi[, , i] %*% x$SR
  }
  return(Phi)
}
