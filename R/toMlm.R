#' @export toMlm
toMlm <- function(x, ...) {
  UseMethod("toMlm")
}

#' @export toMlm.default
toMlm.default <- function(x, ...){
  lm(x$model)
}

#' @export toMlm.varest
toMlm.varest<-function(x, ...){
  ix <- 1:x$K
  X<-x$datamat
  type<-x$type
  is.const<-type%in%c("const", "both")
  #remove constant in datamat
  if(is.const) X<-X[, -grep("const", colnames(X))]
  #construct formula
  left <- paste(names(X)[ix], collapse = ",")
  if(is.const) {
    fo <- as.formula(paste("cbind(", left, ") ~ ."))
  } else {
    fo <- as.formula(paste("cbind(", left, ") ~ .-1")) #remove automatical constant
  }
  #apply lm
  res<-eval(substitute(lm(fo, X), list(fo = fo))) #code suggested by Gabor Groothendick
  return(res)
}

#' @importFrom lmtest coeftest
#' @export coeftest.varest
coeftest.varest<-function(x, ...){
  coeftest(toMlm.varest(x), ...)
}

#' @importFrom sandwich bread
#' @export bread.varest
bread.varest<-function(x, ...){
  bread(toMlm.varest(x), ...)
}

#' @import sandwich
#' @export vcov.varest
vcov.varest<-function(object, ...){
  vcov(toMlm.varest(object), ...)
}

#' @importFrom sandwich vcovHC
#' @export vcovHC.varest
vcovHC.varest<-function(x, ...){
  vcovHC(toMlm.varest(x), ...)
}

#' @export estfun.varest
estfun.varest<-function(x, ...){
  estfun(toMlm.varest(x), ...)
}
