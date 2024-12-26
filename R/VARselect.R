#' Information criteria and FPE for different VAR(p)
#' 
#' The function returns infomation criteria and final prediction error for
#' sequential increasing the lag order up to a VAR(p)-proccess.  which are
#' based on the same sample size.
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
#' \code{season} and \code{exogen} the default is set to \code{NULL}.\cr Based
#' on the same sample size the following information criteria and the final
#' prediction error are computed: \deqn{ AIC(n) = \ln \det(\tilde{\Sigma}_u(n))
#' + \frac{2}{T}n K^2 \quad, } \deqn{ HQ(n) = \ln \det(\tilde{\Sigma}_u(n)) +
#' \frac{2 \ln(\ln(T))}{T}n K^2 \quad, } \deqn{ SC(n) = \ln
#' \det(\tilde{\Sigma}_u(n)) + \frac{\ln(T)}{T}n K^2 \quad, }
#' 
#' \deqn{ FPE(n) = \left ( \frac{T + n^*}{T - n^*} \right )^K
#' \det(\tilde{\Sigma}_u(n)) \quad , } with \eqn{\tilde{\Sigma}_u (n) = T^{-1}
#' \sum_{t=1}^T \bold{\hat{u}}_t \bold{\hat{u}}_t'} and \eqn{n^*} is the total
#' number of the parameters in each equation and \eqn{n} assigns the lag order.
#' 
#' @param y Data item containing the endogenous variables (ts, df, and tibble).
#' Note that the function will only use the numeric columns of a df or a tbl by
#' "select_if(y, is.numeric)".
#' @param lag.max Integer for the highest lag order (default is \code{lag.max =
#' 10}).
#' @param type Type of deterministic regressors to include.
#' @param season Inlusion of centered seasonal dummy variables (integer value
#' of frequency).
#' @param exogen Inlusion of exogenous variables.
#' @return A list with the following elements:\cr
#' 
#' \item{selection}{Vector with the optimal lag number according to each
#' criterium.} \item{criteria}{A matrix containing the values of the criteria
#' up to \code{lag.max}.}
#' @author Bernhard Pfaff
#' @seealso \code{\link{VAR}}
#' @references
#' 
#' Akaike, H. (1969), Fitting autoregressive models for prediction,
#' \emph{Annals of the Institute of Statistical Mathematics}, \bold{21}:
#' 243-247.
#' 
#' Akaike, H. (1971), Autoregressive model fitting for control, \emph{Annals of
#' the Institute of Statistical Mathematics}, \bold{23}: 163-180.
#' 
#' Akaike, H. (1973), Information theory and an extension of the maximum
#' likelihood principle, in B. N. Petrov and F. Csáki (eds.), \emph{2nd
#' International Symposium on Information Theory}, Académia Kiadó, Budapest,
#' pp. 267-281.
#' 
#' Akaike, H. (1974), A new look at the statistical model identification,
#' \acronym{IEEE} \emph{Transactions on Automatic Control}, \bold{AC-19}:
#' 716-723.
#' 
#' Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton University
#' Press, Princeton.
#' 
#' Hannan, E. J. and B. G. Quinn (1979), The determination of the order of an
#' autoregression, \emph{Journal of the Royal Statistical Society}, \bold{B41}:
#' 190-195.
#' 
#' Lütkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
#' Analysis}, Springer, New York.
#' 
#' Quinn, B. (1980), Order determination for a multivariate autoregression,
#' \emph{Journal of the Royal Statistical Society}, \bold{B42}: 182-185.
#' 
#' Schwarz, G. (1978), Estimating the dimension of a model, \emph{Annals of
#' Statistics}, \bold{6}: 461-464.
#' @keywords regression
#' @examples
#' 
#'\donttest{
#' data(Canada)
#' VARselect(Canada, lag.max = 5, type="const")
#' 
#' data(Canada_tbl)
#' VARselect(Canada_tbl, lag.max = 5, type="const")
#' }
#' 
#' @export
#' @import stats
#'

"VARselect" <-
function (y, lag.max = 10, type = c("const", "trend", "both",
    "none"), season = NULL, exogen = NULL)
{
# Original code
#    y <- as.matrix(y)
  # Modification for tibble and df
  if (is.ts(y)) {
    #print("ts data")
    as.matrix(y) -> y
  } else if (is.data.frame(y) || tibble::is_tibble(y)){
    # https://www.geeksforgeeks.org/select-only-numeric-columns-from-dataframe-in-r/
    #print("df or tibble")
    dplyr::select_if(y, is.numeric) -> y_tmp
    as.matrix(y_tmp) -> y
  } else {
    # Do nothing
  }
  
  if (any(is.na(y)))
    stop("\nNAs in y.\n")
  
  colnames(y) <- make.names(colnames(y))
  K <- ncol(y)
  lag.max <- abs(as.integer(lag.max))
  type <- match.arg(type)
  lag <- abs(as.integer(lag.max + 1))
  ylagged <- embed(y, lag)[, -c(1:K)]
  yendog <- y[-c(1:lag.max), ]
  sample <- nrow(ylagged)
  
  rhs <- switch(type, const = rep(1, sample), trend = seq(lag.max + 1,
        length = sample), both = cbind(rep(1, sample), 
        seq(lag.max + 1, length = sample)), none = NULL)

    if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < sample) {
      dums <- rbind(dums, dum)
      }
    dums <- dums[1:sample, ]
    rhs <- cbind(rhs, dums)
  }
  
  if (!(is.null(exogen))) {
    exogen <- as.matrix(exogen)
    if (!identical(nrow(exogen), nrow(y))) {
      stop("\nDifferent row size of y and exogen.\n")
      }
    if (is.null(colnames(exogen))) {
      colnames(exogen) <- paste("exo", 1:ncol(exogen),sep = "")
      warning(paste("No column names supplied in exogen, using:",
                    paste(colnames(exogen), collapse = ", "), ", instead.\n"))
      }
    colnames(exogen) <- make.names(colnames(exogen))
    rhs <- cbind(rhs, exogen[-c(1:lag.max), ])
  }
  
  idx <- seq(K, K * lag.max, K)
  if(!is.null(rhs)){
    detint <- ncol(as.matrix(rhs))
    } else {
      detint <- 0
    }
  
  criteria <- matrix(NA, nrow = 4, ncol = lag.max)
  rownames(criteria) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")
  colnames(criteria) <- paste(seq(1:lag.max))
  for (i in 1:lag.max) {
    ys.lagged <- cbind(ylagged[, c(1:idx[i])], rhs)
    sampletot <- nrow(y)
    nstar <- ncol(ys.lagged)
    resids <- lm.fit(x=ys.lagged, y=yendog)$residuals
    sigma.det <- det(crossprod(resids)/sample)
    criteria[1, i] <- log(sigma.det) + (2/sample) * (i * K^2 + K * detint)
    criteria[2, i] <- log(sigma.det) + (2 * log(log(sample))/sample) * (i * K^2 + K * detint)
    criteria[3, i] <- log(sigma.det) + (log(sample)/sample) * (i * K^2 + K * detint)
    criteria[4, i] <- ((sample + nstar)/(sample - nstar))^K * sigma.det
  }
  
  order <- apply(criteria, 1, which.min)
  return(list(selection = order, criteria = criteria))
}

