#' irf: Impulse responses for varest
#' @importFrom methods is
#' @importFrom utils head tail
#' @export
"SVAR" <-
function (x, estmethod = c("scoring", "direct"), Amat = NULL,
    Bmat = NULL, start = NULL, max.iter = 100, conv.crit = 1e-07,
    maxls = 1, lrtest = TRUE, ...)
{
    if (!is(x, "varest")) {
        stop("\nPlease, provide an object of class 'varest',\n generated by function 'VAR()' as input for 'x'.\n")
    }
    call <- match.call()
    estmethod <- match.arg(estmethod)
    if ((is.null(Amat)) && (is.null(Bmat))) {
        stop("\nAt least one matrix, either 'Amat' or 'Bmat', must be non-null.\n")
    }
    if ((is.null(Amat)) && !(is.null(Bmat))) {
        Amat <- diag(x$K)
        svartype <- "B-model"
    }
    else if ((is.null(Bmat)) && !(is.null(Amat))) {
        Bmat <- diag(x$K)
        svartype <- "A-model"
    }
    else {
        svartype <- "AB-model"
        diag(Amat) <- 1
        freeA <- which(is.na(c(Amat)))
        freeB <- which(is.na(c(Bmat)))
        if (any(freeA %in% freeB)) {
            stop("\nSVAR not identified. Free parameters at the same positions in Amat and Bmat.\n")
        }
    }
    if (!any(is.na(cbind(Amat, Bmat)))) {
        stop("\nNo parameters provided for optimisation, i.e.\nneither 'Amat' nor 'Bmat' do contain NA elements.\n")
    }
    K <- x$K
    obs <- x$obs
    df <- summary(x$varresult[[1]])$df[2]
    sigma <- crossprod(resid(x)) / df
    params.A <- sum(is.na(Amat))
    params.B <- sum(is.na(Bmat))
    params <- params.A + params.B
    if ((svartype == "B-model") || (svartype == "A-model")) {
        if (K^2 - params < K * (K - 1)/2) {
            stop("\nModel is not identified,\nchoose different settings for 'Amat' and/or 'Bmat'.\n")
        }
    }
    else if (svartype == "AB-model") {
        if (2 * K^2 - params < K^2 + K * (K - 1)/2) {
            stop("\nModel is not identified,\nchoose different settings for 'Amat' and/or 'Bmat'.\n")
        }
    }
    if (is.null(start))
        start <- rep(0.1, params)
    start <- as.vector(start)
    if (!(length(start) == params)) {
        stop("\nWrong count of starting values provided in 'start'.\nLength of 'start' must be equal to the count of 'na' in 'Amat' and 'Bmat'.\n")
    }
    if (estmethod == "direct") {
        param.Aidx <- which(is.na(Amat), arr.ind = TRUE)
        param.Bidx <- which(is.na(Bmat), arr.ind = TRUE)
        logLc <- function(coef) {
            if (svartype == "B-model") {
                Bmat[param.Bidx] <- coef
            }
            else if (svartype == "A-model") {
                Amat[param.Aidx] <- coef
            }
            else if (svartype == "AB-model") {
                if (length(param.Aidx) > 0) {
                  Amat[param.Aidx] <- coef[c(1:nrow(param.Aidx))]
                  if (length(param.Bidx) > 0) {
                    Bmat[param.Bidx] <- coef[-c(1:nrow(param.Aidx))]
                  }
                }
                else if (length(param.Aidx) == 0) {
                  Bmat[param.Bidx] <- coef
                }
            }
            logLc <- -1 * (K * obs/2) * log(2 * pi) + obs/2 *
                log(det(Amat)^2) - obs/2 * log(det(Bmat)^2) -
                obs/2 * sum(diag(t(Amat) %*% solve(t(Bmat)) %*%
                  solve(Bmat) %*% Amat %*% sigma))
            return(-logLc)
        }
        opt <- optim(start, logLc, ...)
        iter <- opt$counts[1]
        Asigma <- matrix(0, nrow = K, ncol = K)
        Bsigma <- matrix(0, nrow = K, ncol = K)
        if (!(is.null(opt$hessian))) {
            Sigma <- sqrt(diag(solve(opt$hessian)))
        }
        if (svartype == "B-model") {
            Bmat[param.Bidx] <- opt$par
            if (!(is.null(opt$hessian))) {
                Bsigma[param.Bidx] <- Sigma
            }
        }
        else if (svartype == "A-model") {
            Amat[param.Aidx] <- opt$par
            if (!(is.null(opt$hessian))) {
                Asigma[param.Aidx] <- Sigma
            }
        }
        else if (svartype == "AB-model") {
            if (length(param.Aidx) > 0) {
                Amat[param.Aidx] <- head(opt$par, nrow(param.Aidx))
                if (!(is.null(opt$hessian))) {
                  Asigma[param.Aidx] <- head(Sigma, nrow(param.Aidx))
                }
            }
            else {
                Amat <- Amat
            }
            if (length(param.Bidx) > 0) {
                Bmat[param.Bidx] <- tail(opt$par, nrow(param.Bidx))
                if (!(is.null(opt$hessian))) {
                  Bsigma[param.Bidx] <- tail(Sigma, nrow(param.Bidx))
                }
            }
            else {
                Bmat <- Bmat
            }
        }
    }
    if (estmethod == "scoring") {
        gamma <- start
        Ksq <- K^2
        if (svartype == "A-model") {
            rb <- c(diag(K))
            ra <- c(Amat)
            pos <- which(is.na(ra))
            cols <- length(pos)
            Ra <- matrix(0, nrow = Ksq, ncol = cols)
            for (i in 1:cols) Ra[pos[i], i] <- 1
            ra[pos] <- 0
        }
        if (svartype == "B-model") {
            ra <- c(diag(K))
            rb <- c(Bmat)
            pos <- which(is.na(rb))
            cols <- length(pos)
            Rb <- matrix(0, nrow = Ksq, ncol = cols)
            for (i in 1:cols) Rb[pos[i], i] <- 1
            rb[pos] <- 0
        }
        if (svartype == "AB-model") {
            ra <- c(Amat)
            pos <- which(is.na(ra))
            cols <- length(pos)
            Ra <- matrix(0, nrow = Ksq, ncol = cols)
            for (i in 1:cols) Ra[pos[i], i] <- 1
            ra[pos] <- 0
            rb <- c(Bmat)
            pos <- which(is.na(rb))
            cols <- length(pos)
            Rb <- matrix(0, nrow = Ksq, ncol = cols)
            for (i in 1:cols) Rb[pos[i], i] <- 1
            rb[pos] <- 0
        }
        R <- matrix(0, nrow = 2 * Ksq, ncol = params)
        if (identical(as.integer(params.A), as.integer(0))) {
            R[(Ksq + 1):(2 * Ksq), 1:params] <- Rb
        }
        else if (identical(as.integer(params.B), as.integer(0))) {
            R[1:Ksq, 1:params] <- Ra
        }
        else if ((!(is.null(params.A)) && (!(is.null(params.B))))) {
            R[1:Ksq, 1:params.A] <- Ra
            R[(Ksq + 1):(2 * Ksq), (params.A + 1):params] <- Rb
        }
        r <- c(ra, rb)
        Kkk <- diag(Ksq)[, c(sapply(1:K, function(i) seq(i, Ksq,
            K)))]
        IK2 <- diag(Ksq)
        IK <- diag(K)
        iters <- 0
        cvcrit <- conv.crit + 1
        while (cvcrit > conv.crit) {
            z <- gamma
            vecab <- R %*% gamma + r
            Amat <- matrix(vecab[1:Ksq], nrow = K, ncol = K)
            Bmat <- matrix(vecab[(Ksq + 1):(2 * Ksq)], nrow = K,
                ncol = K)
            Binv <- solve(Bmat)
            Btinv <- solve(t(Bmat))
            BinvA <- Binv %*% Amat
            infvecab.mat1 <- rbind(kronecker(solve(BinvA), Btinv),
                -1 * kronecker(IK, Btinv))
            infvecab.mat2 <- IK2 + Kkk
            infvecab.mat3 <- cbind(kronecker(t(solve(BinvA)),
                Binv), -1 * kronecker(IK, Binv))
            infvecab <- obs * (infvecab.mat1 %*% infvecab.mat2 %*%
                infvecab.mat3)
            infgamma <- t(R) %*% infvecab %*% R
            infgammainv <- solve(infgamma)
            scorevecBinvA <- obs * c(solve(t(BinvA))) - obs *
                (kronecker(sigma, IK) %*% c(BinvA))
            scorevecAB.mat <- rbind(kronecker(IK, Btinv), -1 *
                kronecker(BinvA, Btinv))
            scorevecAB <- scorevecAB.mat %*% scorevecBinvA
            scoregamma <- t(R) %*% scorevecAB
            direction <- infgammainv %*% scoregamma
            length <- max(abs(direction))
            ifelse(length > maxls, lambda <- maxls/length, lambda <- 1)
            gamma <- gamma + lambda * direction
            iters <- iters + 1
            z <- z - gamma
            cvcrit <- max(abs(z))
            if (iters >= max.iter) {
                warning(paste("Convergence not achieved after",
                  iters, "iterations. Convergence value:", cvcrit,
                  "."))
                break
            }
        }
        iter <- iters - 1
        abSigma <- sqrt(diag((R %*% solve(infgamma) %*% t(R))))
        Asigma <- matrix(abSigma[1:Ksq], nrow = K, ncol = K)
        Bsigma <- matrix(abSigma[(Ksq + 1):(2 * Ksq)], nrow = K,
            ncol = K)
        opt <- NULL
    }
    colnames(Amat) <- colnames(x$y)
    rownames(Amat) <- colnames(Amat)
    colnames(Bmat) <- colnames(Amat)
    rownames(Bmat) <- colnames(Amat)
    colnames(Asigma) <- colnames(Amat)
    rownames(Asigma) <- colnames(Amat)
    colnames(Bsigma) <- colnames(Amat)
    rownames(Bsigma) <- colnames(Amat)
    ##
    ## Normalize sign of Amat and Bmat if applicable
    ##
    if(svartype == "AB-model"){
      if(any(diag(Amat) < 0)){
        ind <- which(diag(Amat) < 0)
        Amat[, ind] <- -1 * Amat[, ind]
      }
      if(any(diag(Bmat) < 0)){
        ind <- which(diag(Bmat) < 0)
        Bmat[, ind] <- -1 * Bmat[, ind]
      }
    }
    if(svartype == "B-model"){
      if(any(diag(solve(Amat) %*% Bmat) < 0)){
        ind <- which(diag(solve(Amat) %*% Bmat) < 0)
        Bmat[, ind] <- -1 * Bmat[, ind]
      }
    }
    if(svartype == "A-model"){
      if(any(diag(solve(Amat) %*% Bmat) < 0)){
        ind <- which(diag(solve(Amat) %*% Bmat) < 0)
        Amat[, ind] <- -1 * Amat[, ind]
      }

    }
    Sigma.U <- solve(Amat) %*% Bmat %*% t(Bmat) %*% t(solve(Amat))
    LRover <- NULL
    if (lrtest) {
        degrees <- 2 * K^2 - params - 2 * K^2 + 0.5 * K * (K +
            1)
        if (identical(degrees, 0)) {
            warning(paste("The", svartype, "is just identified. No test possible."))
        }
        else {
            STATISTIC <- obs * (log(det(Sigma.U)) - log(det(sigma)))
            names(STATISTIC) <- "Chi^2"
            PARAMETER <- 2 * K^2 - params - 2 * K^2 + 0.5 * K *
                (K + 1)
            names(PARAMETER) <- "df"
            PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
            METHOD <- "LR overidentification"
            LRover <- list(statistic = STATISTIC, parameter = PARAMETER,
                p.value = PVAL, method = METHOD, data.name = x$call$y)
            class(LRover) <- "htest"
        }
    }
    result <- list(A = Amat, Ase = Asigma, B = Bmat, Bse = Bsigma,
        LRIM = NULL, Sigma.U = Sigma.U * 100, LR = LRover, opt = opt,
        start = start, type = svartype, var = x, iter = iter, call = call)
    class(result) <- "svarest"
    return(result)
}
