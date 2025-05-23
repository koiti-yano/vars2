#' @export

"arch.test" <-
function(x, lags.single = 16, lags.multi = 5, multivariate.only = TRUE){
  if(!(is(x, "varest") || is(x, "vec2var"))){
    stop("\nPlease provide an object of class 'varest', generated by 'var()', or an object of class 'vec2var' generated by 'vec2var()'.\n")
  }
  obj.name <- deparse(substitute(x))
  lags.single <- abs(as.integer(lags.single))
  lags.multi <- abs(as.integer(lags.multi))
  K <- x$K
  obs <- x$obs
  resid <- resid(x)
  resids <- scale(resid)
  ## ARCH test (multivariate)
  archm.resids <- .arch.multi(resids, lags.multi = lags.multi, K = K, obs = obs, obj.name = obj.name)
  if(multivariate.only){
    result <- list(resid=resid, arch.mul = archm.resids)
  } else {
    ## ARCH test (univariate)
    archs.resids <- apply(resids, 2, function(x) .arch.uni(x, lags.single = lags.single))
    for(i in 1 : K)
      archs.resids[[i]][5] <- paste("Residual of", colnames(resids)[i], "equation")
    result <- list(resid=resid, arch.uni = archs.resids, arch.mul = archm.resids)
  }
  class(result) <- "varcheck"
  return(result)
}
arch <- function(x, lags.single = 16, lags.multi = 5, multivariate.only = TRUE){
  .Deprecated("arch.test", package = "vars", msg = "Function 'arch' is deprecated; use 'arch.test' instead.\nSee help(\"vars-deprecated\") and help(\"arch-deprecated\") for more information.")
  arch.test(x = x, lags.single = lags.single, lags.multi = lags.multi, multivariate.only = multivariate.only)
}
