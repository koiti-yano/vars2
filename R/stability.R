#' Structural stability of a VAR(p)
#'
#' Computes an empirical fluctuation process according to a specified method
#' from the generalised fluctuation test framework. The test utilises the
#' function \command{efp()} and its methods from package
#' \sQuote{\code{strucchange}}.
#'
#' For details, please refer to documentation \code{\link[strucchange]{efp}}.
#'
#' @aliases stability stability.default stability.varest print.varstabil
#' @param x Object of class \sQuote{\code{varest}}; generated by
#' \command{VAR()}.
#' @param type Specifies which type of fluctuation process will be computed,
#' the default is \sQuote{\code{OLS-CUSUM}}. For details see:
#' \code{\link[strucchange]{efp}}.
#' @param h A numeric from interval (0,1) sepcifying the bandwidth. Determins
#' the size of the data window relative to sample size (for
#' \sQuote{\code{MOSUM}} and \sQuote{\code{ME}} processes only).
#' @param dynamic Logical. If \sQuote{\code{TRUE}} the lagged observations are
#' included as a regressor.
#' @param rescale Logical. If \sQuote{\code{TRUE}} the estimates will be
#' standardized by the regressor matrix of the corresponding subsample; if
#' \sQuote{\code{FALSE}} the whole regressor matrix will be used. (only if
#' \sQuote{\code{type}} is either \sQuote{\code{RE}} or \sQuote{\code{E}}).
#' @param ... Ellipsis, is passed to \code{strucchange::sctest()}, as default.
#' @return A list with class attribute \sQuote{\code{varstabil}} holding the
#' following elements:\cr
#'
#' \item{stability}{A list with objects of class \sQuote{\code{efp}}; length is
#' equal to the dimension of the VAR.} \item{names}{Character vector containing
#' the names of the endogenous variables.} \item{K}{An integer of the VAR
#' dimension.}
#' @author Bernhard Pfaff
#' @seealso \code{\link{VAR}}, \code{\link{plot}},
#' \code{\link[strucchange]{efp}}
#' @references Zeileis, A., F. Leisch, K. Hornik and C. Kleiber (2002),
#' strucchange: An R Package for Testing for Structural Change in Linear
#' Regression Models, \emph{Journal of Statistical Software}, \bold{7(2)}:
#' 1-38, \url{https://www.jstatsoft.org/v07/i02/}
#'
#' and see the references provided in the reference section of
#' \code{\link[strucchange]{efp}}, too.
#' @keywords regression
#' @examples
#'
#' data(Canada)
#' var.2c <- VAR(Canada, p = 2, type = "const")
#' var.2c.stabil <- stability(var.2c, type = "OLS-CUSUM")
#' var.2c.stabil
#' plot(var.2c.stabil)
#'
#' @importFrom strucchange efp
#' @export
"stability" <-
function(x, ...){
  UseMethod("stability")
}

#' @export
"stability.default" <-
    function(x, type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM", "OLS-MOSUM", "RE",
                         "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
             h = 0.15, dynamic = FALSE, rescale = TRUE, ...){
        strucchange::sctest(x, ...)
}

#' @export
"stability.varest" <-
    function(x, type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM", "OLS-MOSUM", "RE",
                         "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
             h = 0.15, dynamic = FALSE, rescale = TRUE, ...){
  type <- match.arg(type)
  K <- x$K
  stability <- list()
  endog <- colnames(x$datamat)[1 : K]
  for(i in 1 : K){
    formula <- formula(x$varresult[[i]])
    data <- x$varresult[[i]]$model
    stability[[endog[i]]] <- strucchange::efp(formula = formula, data = data, type = type, h = h, dynamic = dynamic, rescale = rescale)
  }
  result <- list(stability = stability, names = endog, K = K)
  class(result) <- "varstabil"
  return(result)
}
