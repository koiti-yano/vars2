#' Plot impulse responses with ggplot2
#'
#' @param irf impulse responses of VAR or SVAR
#' @param \dots further arguments passed to or from other methods 
#' (currently not used).
#' 
#' @export
"vars_plot" <- function(irf, ...){
  UseMethod("vars_plot", irf)
}
