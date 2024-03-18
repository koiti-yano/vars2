#' VARselect: Information criteria for different VAR(p)
#'
#' @param y Data item containing the endogenous variables
#' @param lag.max Integer, determines the highest lag order for lag length selection according to the choosen ic
#' @param type Type of deterministic regressors to include.
#' @param season Inlusion of centered seasonal dummy variables (integer value of frequency).
#' @param exogen Inlusion of exogenous variables.
#'
#' @return list
#' @export
#' @import stats
#' @importFrom dplyr select_if
#' @importFrom tibble is_tibble
#'

"VARselect" <-
function (y, lag.max = 10, type = c("const", "trend", "both",
    "none"), season = NULL, exogen = NULL)
{
    y <- as.matrix(y)
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
        length = sample), both = cbind(rep(1, sample), seq(lag.max + 1, length = sample)), none = NULL)
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
            colnames(exogen) <- paste("exo", 1:ncol(exogen),
                sep = "")
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
