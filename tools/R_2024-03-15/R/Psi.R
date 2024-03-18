#' Psi: generic function
#' @import stats
#' @export
"Psi" <-
function(x, nstep=10, ...){
  UseMethod("Psi", x)
}

