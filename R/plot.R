#' Plot methods for objects in vars
#'
#' Plot method for objects with class attribute \code{varest}, \code{vec2var},
#' \code{varcheck}, \code{varfevd}, \code{varirf}, \code{varprd},
#' \code{varstabil}.
#'
#' The plot-method for objects with class attribute \code{vec2var} is the same
#' as for objects with class attribute \code{varest}. Hence, the same arguments
#' can be utilised.
#'
#' @usage 
#' ## S3 method for class 'varcheck'
#' plot(x, names = NULL, main.resid = NULL, main.hist = NULL,
#'    main.acf = NULL, main.pacf = NULL, main.acf2 = NULL, 
#'    main.pacf2 =NULL, ylim.resid = NULL, ylim.hist = NULL, 
#'    ylab.resid = NULL, xlab.resid = NULL, xlab.acf = NULL, 
#'    lty.resid = NULL, lwd.resid = NULL, col.resid = NULL,
#'    col.edf = NULL, lag.acf = NULL, lag.pacf = NULL, lag.acf2 = NULL,
#'    lag.pacf2 = NULL, mar = par("mar"), oma = par("oma"), ...)
#'    
#' ## S3 method for class 'varest'
#' plot(x, names = NULL, main.fit = NULL, main.acf = NULL,
#'    main.pacf = NULL, ylim.fit = NULL, ylim.resid = NULL,
#'    lty.fit = NULL, lty.resid = NULL, lwd.fit = NULL, 
#'    lwd.resid = NULL, lag.acf = NULL, lag.pacf = NULL, 
#'    col.fit = NULL, col.resid = NULL, ylab.fit = NULL,
#'    ylab.resid = NULL, ylab.acf = NULL, ylab.pacf = NULL, 
#'    xlab.fit = NULL, xlab.resid = NULL, nc, mar = par("mar"), 
#'    oma = par("oma"), adj.mtext = NA, padj.mtext = NA, col.mtext = NA, ...)
#'    
#' ## S3 method for class 'vec2var'
#' plot(x, ...)
#' 
#' ## S3 method for class 'varfevd'
#' plot(x, plot.type = c("multiple", "single"), names = NULL,
#'    main = NULL, col = NULL, ylim = NULL, ylab = NULL,
#'    xlab = NULL, legend = NULL, names.arg = NULL, nc,
#'    mar = par("mar"), oma = par("oma"), addbars = 1, ...)
#'    
#' ## S3 method for class 'varirf'
#' plot(x, plot.type = c("multiple", "single"), names =
#'    NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, 
#'    col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, 
#'    nc, mar.multi = c(0, 4, 0, 4), oma.multi = c(6, 4, 6, 4),
#'    adj.mtext = NA, padj.mtext = NA, col.mtext = NA, ...)
#'      
#' ## S3 method for class 'varprd'
#' plot(x, plot.type = c("multiple", "single"), names = NULL,
#'    main = NULL, col = NULL, lty = NULL, lwd = NULL,
#'    ylim = NULL, ylab = NULL, xlab = NULL, nc, mar = par("mar"),
#'    oma = par("oma"), ...)
#'    
#' ## S3 method for class 'varstabil'
#' plot(x, plot.type = c("multiple", "single"), names = NULL,
#'    main = NULL, nc, mar = par("mar"), oma = par("oma"), ...)
#'    
#' @name plot
#' @aliases plot.varcheck plot.varest plot.varfevd plot.varirf plot.varprd
#' plot.varstabil plot.vec2var
#' @param addbars Integer, number of empty bars in barplot to reserve space for
#' legend. If set to zero, no legend will be returned.
#' @param adj.mtext Adjustment for \code{mtext()}, only applicable if
#' \code{plot.type = "multiple"}.
#' @param col Character vector, colors to use in plot.
#' @param col.edf Character, color of residuals' EDF.
#' @param col.fit Character vector, colors for diagram of fit.
#' @param col.mtext Character, color for \code{mtext()}, only applicable if
#' \code{plot.type = "multiple"}.
#' @param col.resid Character vector, colors for residual plot.
#' @param lag.acf Integer, lag.max for ACF of residuals.
#' @param lag.acf2 Integer, lag.max for ACF of squared residuals.
#' @param lag.pacf Integer, lag.max for PACF of residuals.
#' @param lag.pacf2 Integer, lag.max for PACF of squared residuals.
#' @param legend Character vector of names in legend.
#' @param lty Integer/Character, the line types.
#' @param lty.fit Vector, lty for diagram of fit.
#' @param lty.resid Vector, lty for residual plot.
#' @param lwd The width of the lines.
#' @param lwd.fit Vector, lwd for diagram of fit.
#' @param lwd.resid Vector, lwd for residual plot.
#' @param main Character vector, the titles of the plot.
#' @param main.acf Character vector, main for residuals' ACF.
#' @param main.acf2 Character vector, main for squared residuals' ACF.
#' @param main.fit Character vector, main for diagram of fit.
#' @param main.hist Character vector, main for histogram of residuals.
#' @param main.pacf Character vector, main for residuals' PACF.
#' @param main.pacf2 Character vector, main for squared residuals' PACF.
#' @param main.resid Character vector, main for residual plot.
#' @param mar Setting of margins.
#' @param mar.multi Setting of margins, if \code{plot.type = "multiple"}.
#' @param names Character vector, the variables names to be plotted. If left
#' \code{NULL}, all variables are plotted.
#' @param names.arg Character vector, names for x-axis of barplot.
#' @param nc Integer, number of columns for multiple plot.
#' @param oma Setting of outer margins.
#' @param oma.multi Setting of margins, if \code{plot.type = "multiple"}.
#' @param padj.mtext Adjustment for \code{mtext()}, only applicable if
#' \code{plot.type = "multiple"}.
#' @param plot.type Character, if \code{multiple} all plots are drawn in a
#' single device, otherwise the plots are shown consecutively.
#' @param sub Character, sub title in plot.
#' @param x An object of one of the above classes.
#' @param xlab Character vector signifying the labels for the x-axis.
#' @param xlab.acf Character, xlab for ACF and PACF of residuals and their
#' squares in plot.varcheck.
#' @param xlab.fit Character vector, xlab for diagram of fit.
#' @param xlab.resid Character vector, xlab for residual plot.
#' @param ylab Character vector signifying the labels for the y-axis.
#' @param ylab.acf Character, ylab for ACF.
#' @param ylab.fit Character vector, ylab for diagram of fit.
#' @param ylab.pacf Character, ylab for PACF
#' @param ylab.resid Character vector, ylab for residual plot.
#' @param ylim Vector, the limits of the y-axis.
#' @param ylim.fit Vector, ylim for diagram of fit.
#' @param ylim.hist Vector, ylim for histogram of residuals.
#' @param ylim.resid Vector, ylim for residual plot.
#' @param \dots Passed to internal plot function.
#' @author Bernhard Pfaff
#' @seealso \code{\link{VAR}}, \code{\link{vec2var}}, \code{\link{fevd}},
#' \code{\link{irf}}, \code{\link{predict}}, \code{\link{fanchart}},
#' \code{\link{stability}}, \code{\link{arch.test}},
#' \code{\link{normality.test}}, \code{\link{serial.test}}
#' @references Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton
#' University Press, Princeton.
#'
#' Lütkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
#' Analysis}, Springer, New York.
#'
#' Zeileis, A., F. Leisch, K. Hornik and C. Kleiber (2002), strucchange: An R
#' Package for Testing for Structural Change in Linear Regression Models,
#' \emph{Journal of Statistical Software}, \bold{7(2)}: 1-38,
#' \url{https://www.jstatsoft.org/v07/i02/}
#' @keywords regression
#' @examples
#'
#' \dontrun{
#' data(Canada)
#' var.2c <- VAR(Canada, p = 2, type = "const")
#' plot(var.2c)
#' ## Diagnostic Testing
#' ## ARCH test
#' archtest <- arch.test(var.2c)
#' plot(archtest)
#' ## Normality test
#' normalitytest <- normality.test(var.2c)
#' plot(normalitytest)
#' ## serial correlation test
#' serialtest <- serial.test(var.2c)
#' plot(serialtest)
#' ## FEVD
#' var.2c.fevd <- fevd(var.2c, n.ahead = 5)
#' plot(var.2c.fevd)
#' ## IRF
#' var.2c.irf <- irf(var.2c, impulse = "e",
#' response = c("prod", "rw", "U"), boot = FALSE)
#' plot(var.2c.irf)
#' ## Prediction
#' var.2c.prd <- predict(var.2c, n.ahead = 8, ci = 0.95)
#' plot(var.2c.prd)
#' ## Stability
#' var.2c.stabil <- stability(var.2c, type = "Rec-CUSUM")
#' plot(var.2c.stabil)
#' }
#' @export
"plot" <- function(x, ...){
  UseMethod("plot",x)
}
