#' Transform a VECM to VAR in levels
#'
#' An object of formal class 'ca.jo' is transformed to a VAR in level
#' presentation.
#'
#' This function enables the user to transform a vector-error-correction model
#' (VECM) into a level-VAR form. The rank of the matrix \eqn{\bold{\Pi}} has to
#' be submitted, \emph{i.e.} how many cointegration relationships have been
#' determined according to the outcome of \command{ca.jo()}.
#'
#' @aliases vec2var print.vec2var
#' @param z An object of class 'ca.jo' generated by function \command{ca.jo()}
#' in package 'urca'.
#' @param r The cointegration rank (default is \code{r=1)}.
#' @return A list with class attribute \sQuote{\code{vec2var}} holding the
#' following elements:\cr
#'
#' \item{deterministic}{The matrix of deterministic coefficients.} \item{A}{A
#' list with matrix object(s) containing the coefficients for the lagged
#' endogenous variables.} \item{p}{The lag-order of the estimated VAR-process.}
#' \item{K}{The count of endogenous variables.} \item{y}{A dataframe with the
#' endogenous variables in levels.} \item{obs}{An integer signifying the count
#' of used observations.} \item{totobs}{An integer signifying the total number
#' of observations, \emph{i.e} including observations taken as starting
#' values..} \item{call}{The \code{call} to \code{vec2var}.} \item{vecm}{The
#' supplied object \code{z} of formal class \code{ca.jo}.} \item{datamat}{A
#' dataframe with the used dataset.} \item{resid}{A matrix with the residuals
#' from the empirical VAR(p).} \item{r}{Intefer, the assigned co-integration
#' rank from the call.}
#' @author Bernhard Pfaff
#' @seealso \code{\link[urca]{ca.jo}}, \code{\link{predict}},
#' \code{\link{irf}}, \code{\link{fevd}}, \code{\link{Phi}}, \code{\link{Psi}},
#' \code{\link{normality.test}}, \code{\link{arch.test}},
#' \code{\link{serial.test}}, \code{\link{logLik}}, \code{\link{plot}}
#' @references Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton
#' University Press, Princeton.
#'
#' Lütkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
#' Analysis}, Springer, New York.
#' @keywords regression
#' @examples
#' @export vec2var
"vec2var" <-
function(z, r = 1){
  if (!is(z, "ca.jo")) {
    stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
  }
  r <- as.integer(r)
  if(!({1 <= r} && {r < ncol(z@x)})){
    stop(paste("\nThe cointegration rank 'r' must be in the interval [1:", ncol(z@x) - 1, "].\n", sep = ""))
  }
  etc <- z@ZK %*% z@V[, 1:r]
  colnames(etc) <- paste("etc", 1:r, sep ="")
  coeffs <- coef(lm(z@Z0 ~ -1 + etc + z@Z1))
  rownames(coeffs) <- c(colnames(etc), colnames(z@Z1))
  PI <- z@W[, 1:r] %*% t(z@V[, 1:r])
  if(z@ecdet == "const"){
    detcoeffs <- matrix(PI[, z@P + 1], nrow = 1, ncol = ncol(z@x), byrow = TRUE)
    rownames(detcoeffs) <- "constant"
    colnames(detcoeffs) <- colnames(z@x)
    PI <- PI[, -(z@P + 1)]
    rhs <- cbind(1, z@Z1)
    colnames(rhs) <- c("constant", colnames(z@Z1))
  } else if(z@ecdet == "none"){
    detcoeffs <- matrix(coeffs["constant", ], nrow = 1, ncol = ncol(z@x), byrow = TRUE)
    rownames(detcoeffs) <- "constant"
    colnames(detcoeffs) <- colnames(z@x)
    rhs <- z@Z1
  } else if(z@ecdet == "trend"){
    detcoeffs <- matrix(c(coeffs["constant", ], PI[, z@P + 1]), nrow = 2, ncol = ncol(z@x), byrow = TRUE)
    rownames(detcoeffs) <- c("constant", colnames(z@ZK)[z@P + 1])
    colnames(detcoeffs) <- colnames(z@x)
    PI <- PI[, -(z@P + 1)]
    rhs <- cbind(1, z@ZK[, z@P + 1], z@Z1[, -1])
    colnames(rhs) <- c("constant", colnames(z@ZK)[z@P + 1], colnames(z@Z1)[-1])
  }
  if(!(is.null(eval(z@season)))){
    seas <- eval(z@season) - 1
    season <- paste("sd", 1:seas, sep = "")
    detcoeffs <- rbind(detcoeffs, coeffs[season, ])
  }
  if(!(is.null(eval(z@dumvar)))){
    dumnames <- colnames(z@dumvar)
    tmp <- rownames(detcoeffs)
    detcoeffs <- rbind(detcoeffs, coeffs[dumnames, ])
    rownames(detcoeffs) <- c(tmp, dumnames)
  }
  detcoeffs <- t(detcoeffs)
  Gamma <- t(coeffs[- which(rownames(coeffs) %in% c(colnames(detcoeffs), colnames(etc))), ])
  rownames(Gamma) <- colnames(z@x)
  A <- list()
  if(identical(z@spec, "transitory")){
    if(identical(z@lag, as.integer(2))){
      A$A1 <- Gamma + PI + diag(z@P)
      rownames(A$A1) <- colnames(z@x)
      colnames(A$A1) <- paste(colnames(z@x), ".l1", sep = "")
      A$A2 <- -1.0 * Gamma
      rownames(A$A2) <- colnames(z@x)
      colnames(A$A2) <- paste(colnames(z@x), ".l2", sep = "")
    } else if(z@lag > 2){
      idx.end <- seq(from = z@P, by = z@P, length.out = z@lag - 1)
      idx.start <- idx.end - z@P + 1
      A[[1]] <- Gamma[, idx.start[1]:idx.end[1]] + PI + diag(z@P)
      rownames(A[[1]]) <- colnames(z@x)
      colnames(A[[1]]) <- paste(colnames(z@x), ".l1", sep = "")
      for(i in 2:(z@lag - 1)){
        A[[i]] <- Gamma[, idx.start[i]:idx.end[i]] - Gamma[, idx.start[i - 1]:idx.end[i - 1]]
        rownames(A[[i]]) <- colnames(z@x)
        colnames(A[[i]]) <- paste(colnames(z@x), ".l", i, sep = "")
      }
      A[[z@lag]] <- -1.0 * Gamma[, tail(idx.start, 1):tail(idx.end, 1)]
      rownames(A[[z@lag]]) <- colnames(z@x)
      colnames(A[[z@lag]]) <- paste(colnames(z@x), ".l", z@lag, sep = "")
      names(A) <- paste("A", 1:z@lag, sep = "")
    }
  }
  if(identical(z@spec, "longrun")){
    if(identical(z@lag, as.integer(2))){
      A$A1 <- Gamma + diag(z@P)
      rownames(A$A1) <- colnames(z@x)
      colnames(A$A1) <- paste(colnames(z@x), ".l1", sep = "")
      A$A2 <- PI + diag(z@P) - A$A1
      rownames(A$A2) <- colnames(z@x)
      colnames(A$A2) <- paste(colnames(z@x), ".l2", sep = "")
    } else if(z@lag > 2){
      idx.end <- seq(from = z@P, by = z@P, length.out = z@lag - 1)
      idx.start <- idx.end - z@P + 1
      A[[1]] <- Gamma[, idx.start[1]:idx.end[1]] + diag(z@P)
      rownames(A[[1]]) <- colnames(z@x)
      colnames(A[[1]]) <- paste(colnames(z@x), ".l1", sep = "")
      for(i in 2:(z@lag - 1)){
        A[[i]] <- Gamma[, idx.start[i]:idx.end[i]] - Gamma[, idx.start[i - 1]:idx.end[i - 1]]
        rownames(A[[i]]) <- colnames(z@x)
        colnames(A[[i]]) <- paste(colnames(z@x), ".l", i, sep = "")
      }
      A[[z@lag]] <- PI - Gamma[ ,tail(idx.start, 1):tail(idx.end, 1)]
      rownames(A[[z@lag]]) <- colnames(z@x)
      colnames(A[[z@lag]]) <- paste(colnames(z@x), ".l", z@lag, sep = "")
      names(A) <- paste("A", 1:z@lag, sep = "")
    }
  }
  datamat <- embed(z@x, dimension = z@lag + 1)
  datamat <- cbind(datamat[, 1:ncol(z@x)], rhs[, colnames(detcoeffs)], datamat[, -c(1:ncol(z@x))])
  temp1 <- NULL
  for (i in 1:z@lag) {
    temp <- paste(colnames(z@x), ".l", i, sep = "")
    temp1 <- c(temp1, temp)
  }
  colnames(datamat) <- c(colnames(z@x), colnames(detcoeffs), temp1)
  resids <- datamat[, colnames(z@x)] - datamat[, colnames(detcoeffs)] %*% t(detcoeffs)
  for(i in 1:z@lag){
    resids <- resids - datamat[, colnames(A[[i]])] %*% t(A[[i]])
  }
  colnames(resids) <- paste("resids of", colnames(z@x))
  result <- list(deterministic = detcoeffs, A = A, p = z@lag, K = ncol(z@x), y = z@x, obs = nrow(z@Z0), totobs = nrow(z@x), call = match.call(), vecm = z, datamat = datamat, resid = resids, r = r)
  class(result) <- "vec2var"
  return(result)
}
