#' Estimation of a VAR(p)
#'
#' Estimation of a VAR by utilising OLS per equation.
#'
#' Estimates a VAR by OLS per equation. The model is of the following form:
#'
#' \deqn{ \bold{y}_t = A_1 \bold{y}_{t-1} + \ldots + A_p \bold{y}_{t-p} + CD_t
#' + \bold{u}_t }
#'
#' where \eqn{\bold{y}_t} is a \eqn{K \times 1} vector of endogenous variables
#' and \eqn{u_t} assigns a spherical disturbance term of the same dimension.
#' The coefficient matrices \eqn{A_1, \ldots, A_p} are of dimension \eqn{K
#' \times K}. In addition, either a constant and/or a trend can be included as
#' deterministic regressors as well as centered seasonal dummy variables and/or
#' exogenous variables (term \eqn{CD_T}, by setting the \code{type} argument to
#' the corresponding value and/or setting \code{season} to the desired
#' frequency (integer) and/or providing a matrix object for \code{exogen},
#' respectively. The default for \code{type} is \code{const} and for
#' \code{season} and \code{exogen} the default is set to \code{NULL}.\cr If for
#' \code{lag.max} an integer value is provided instead of \code{NULL} (the
#' default), the lag length is determined by the selected information criteria
#' in \code{ic}, the default is Akaike.
#'
#' @aliases VAR print.varest
#' @param y Data item containing the endogenous variables (ts or tibble)
#' @param p Integer for the lag order (default is p=1).
#' @param type Type of deterministic regressors to include.
#' @param season Inlusion of centered seasonal dummy variables (integer value
#' of frequency).
#' @param exogen Inlusion of exogenous variables.
#' @param lag.max Integer, determines the highest lag order for lag length
#' selection according to the choosen \code{ic}.
#' @param ic Character, selects the information criteria, if \code{lag.max} is
#' not \code{NULL}.
#' @return A list with class attribute \sQuote{\code{varest}} holding the
#' following elements:\cr
#'
#' \item{varresult}{list of \sQuote{\code{lm}} objects.} \item{datamat}{The
#' data matrix of the endogenous and explanatory variables.} \item{y}{The data
#' matrix of the endogenous variables} \item{type}{A character, specifying the
#' deterministic regressors.} \item{p}{An integer specifying the lag order.}
#' \item{K}{An integer specifying the dimension of the VAR.} \item{obs}{An
#' integer specifying the number of used observations.} \item{totobs}{An
#' integer specifying the total number of observations.}
#' \item{restrictions}{Either \code{NULL} or a matrix object containing the
#' zero restrictions of the VAR(p).} \item{call}{The \code{call} to
#' \command{VAR()}.}
#' @author Bernhard Pfaff
#' @seealso \code{\link{summary}}, \code{\link{plot}}, \code{\link{coef}},
#' \code{\link{residuals}}, \code{\link{fitted}}, \code{\link{predict}},
#' \code{\link{irf}}, \code{\link{fevd}}, \code{\link{Phi}}, \code{\link{Psi}},
#' \code{\link{normality.test}}, \code{\link{arch.test}},
#' \code{\link{serial.test}}, \code{\link{VARselect}}, \code{\link{logLik}}
#' @references Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton
#' University Press, Princeton.
#'
#' Lütkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
#' Analysis}, Springer, New York.
#' @keywords regression
#' @examples
#'
#' data(Canada)
#' VAR(Canada, p = 2, type = "none")
#' VAR(Canada, p = 2, type = "const")
#' VAR(Canada, p = 2, type = "trend")
#' VAR(Canada, p = 2, type = "both")
#'
#' @importFrom dplyr select_if
#' @importFrom tibble is_tibble
#' @export
"VAR" <-
function (y, p = 1, type = c("const", "trend", "both", "none"),
          season = NULL, exogen = NULL, lag.max = NULL,
          ic = c("AIC", "HQ", "SC", "FPE"))
{
# Original code
#  y <- as.matrix(y)
# Modification for tibble
  if (is.ts(y)) {
#    print("ts data")
    as.matrix(y) -> y
  } else if (tibble::is_tibble(y)){
#    print("tibble")
    dplyr::select_if(y, is.numeric) -> y_tmp
    as.matrix(y_tmp) -> y
#    browser()
#    print(y)
  } else {
    warning("\n Input must be a ts object or a tibble object \n") 
  }

  if (any(is.na(y)))
    stop("\nNAs in y.\n")
  if (ncol(y) < 2)
        stop("The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.\n")
  if (is.null(colnames(y))) {
    colnames(y) <- paste("y", 1:ncol(y), sep = "")
    warning(paste("No column names supplied in y, using:",
                  paste(colnames(y), collapse = ", "), ", instead.\n"))
  }
  colnames(y) <- make.names(colnames(y))
  y.orig <- y
  type <- match.arg(type)
  obs <- dim(y)[1]
  K <- dim(y)[2]
  if(!is.null(lag.max)){
    lag.max <- abs(as.integer(lag.max))
    ic <- paste(match.arg(ic), "(n)", sep = "")
    p <- VARselect(y, lag.max = lag.max, type = type, season = season, exogen = exogen)$selection[ic]
  }
  sample <- obs - p
  ylags <- embed(y, dimension = p + 1)[, -(1:K)]
  temp1 <- NULL

  for (i in 1:p) {
    temp <- paste(colnames(y), ".l", i, sep = "")
    temp1 <- c(temp1, temp)
  }
  colnames(ylags) <- temp1
  yend <- y[-c(1:p), ]
  if (type == "const") {
    rhs <- cbind(ylags, rep(1, sample))
    colnames(rhs) <- c(colnames(ylags), "const")
  }
  else if (type == "trend") {
    rhs <- cbind(ylags, seq(p + 1, length = sample))
    colnames(rhs) <- c(colnames(ylags), "trend")
  }
  else if (type == "both") {
    rhs <- cbind(ylags, rep(1, sample), seq(p + 1, length = sample))
    colnames(rhs) <- c(colnames(ylags), "const", "trend")
  }
  else if (type == "none") {
    rhs <- ylags
    colnames(rhs) <- colnames(ylags)
  }

  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < obs) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:obs, ]
    colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
    rhs <- cbind(rhs, dums[-c(1:p), ])
  }
  if (!(is.null(exogen))) {
    exogen <- as.matrix(exogen)
    if (!identical(nrow(exogen), nrow(y))) {
      stop("\nDifferent row size of y and exogen.\n")
    }
    if (is.null(colnames(exogen))) {
      colnames(exogen) <- paste("exo", 1:ncol(exogen),
                                sep = "")
      warning(paste("No column names supplied in exogen, using:",
                paste(colnames(exogen), collapse = ", "), ", instead.\n"))
    }
    colnames(exogen) <- make.names(colnames(exogen))
    tmp <- colnames(rhs)
    rhs <- cbind(rhs, exogen[-c(1:p), ])
    colnames(rhs) <- c(tmp, colnames(exogen))
  }
  datamat <- as.data.frame(rhs)
  colnames(datamat) <- colnames(rhs)
  equation <- list()
  for (i in 1:K) {
    y <- yend[, i]
    equation[[colnames(yend)[i]]] <- lm(y ~ -1 + ., data = datamat)
    if(any(c("const", "both") %in% type)){
      attr(equation[[colnames(yend)[i]]]$terms, "intercept") <- 1
    }
  }
  call <- match.call()
  if("season" %in% names(call)) call$season <- eval(season)
    result <- list(varresult = equation, datamat = data.frame(cbind(yend,
        rhs)), y = y.orig, type = type, p = p, K = K, obs = sample,
        totobs = sample + p, restrictions = NULL, call = call)
    class(result) <- "varest"
    return(result)
}
