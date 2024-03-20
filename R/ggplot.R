#' Plot method for impulse responses of VAR and SVAR with ggplot2
#'
#' @param irf impulse responses of VAR or SVAR
#' @param sub subtitile of plot (main title is generated automatically)
#' @param cap caption of plot
#' @param \dots further arguments passed to or from other methods 
#' (currently not used).
#' 
#' @examples
#' data(Canada)
#' ## For VAR
#' var.2c <- VAR(Canada, p = 2, type = "const")
#' irf.2c <- irf(var.2c, impulse = "e", response = c("e", "prod", "rw", "U"), boot =
#' TRUE)
#' ggplot(irf.2c, sub="Canada", cap="Caption")
#'
#' ## For SVAR
#' amat <- diag(4)
#' diag(amat) <- NA
#' svar.a <- SVAR(var.2c, estmethod = "direct", Amat = amat)
#' irf.sa <- irf(svar.a)
#' ggplot(irf.sa, sub="Canada", cap="Caption")
#' @export
"ggplot" <- function(irf, ...){
  UseMethod("ggplot", irf)
}
