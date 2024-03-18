#' ggplot: Plot impulse responses of VAR and SVAR with ggplot2
#'
#' @param irf a list of impulse responses
#' @examples
#' data(Canada)
#' ## For VAR
#' var.2c <- VAR(Canada, p = 2, type = "const")
#' irf.2c <- irf(var.2c, impulse = "e", response = c("e", "prod", "rw", "U"), boot =
#' TRUE)
#' ggplot(irf.2c)
#'
#' ## For SVAR
#' amat <- diag(4)
#' diag(amat) <- NA
#' svar.a <- SVAR(var.2c, estmethod = "direct", Amat = amat)
#' irf.sa <- irf(svar.a)
#' ggplot(irf.sa)
#' @export

"ggplot" <- function(irf){
  UseMethod("ggplot")
}
