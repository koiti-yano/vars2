#' @export fitted.varest
"fitted.varest" <-
function(object, ...){
  return(sapply(object$varresult, fitted))
}
