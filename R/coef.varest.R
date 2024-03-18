#' @export coef.varest
"coef.varest" <-
function(object, ...){
  return(lapply(lapply(object$varresult, summary), coef))
}
