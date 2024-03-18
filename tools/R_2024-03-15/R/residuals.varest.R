#' residuals.varest
#' @importFrom stats residuals
#' @export
"residuals.varest" <-
function(object, ...){
  return(sapply(object$varresult, residuals))
}
