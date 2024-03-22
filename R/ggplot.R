#' Plot impulse responses with ggplot2
#'
#' @param irf impulse responses of VAR or SVAR
#' @param \dots further arguments passed to or from other methods 
#' (currently not used).
#' 
#' @export
"ggplot" <- function(irf, ...){
  UseMethod("ggplot", irf)
}
