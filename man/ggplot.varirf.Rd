% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot.varirf.R
\name{ggplot.varirf}
\alias{ggplot.varirf}
\title{Plot impulse responses of VAR and SVAR}
\usage{
\method{ggplot}{varirf}(
  irf,
  main = NULL,
  sub = NULL,
  cap = NULL,
  var_name = NULL,
  dev_new = FALSE,
  ...
)
}
\arguments{
\item{irf}{impulse responses of VAR or SVAR}

\item{main}{main title of plot (The default is NULL, in which case the main
title is generated automatically.)}

\item{sub}{subtitile of plot}

\item{cap}{caption of plot}

\item{var_name}{variable names: ex. var_name=c("Emp", "Prod", "RW", "Unemp")}

\item{dev_new}{logical. If TRUE, open a new graphics device.}

\item{\dots}{further arguments passed to or from other methods 
(currently not used).}
}
\value{
A ggplot object
}
\description{
Plot impulse responses of VAR and SVAR
}
\examples{
\donttest{
data(Canada)
## For VAR
var.2c <- VAR(Canada, p = 2, type = "const")
irf.2c <- irf(var.2c, impulse = "e", response = c("e", "prod", "rw", "U"), boot =
TRUE)
ggplot(irf.2c, sub="Canada", cap="Caption",
var_name=c("Emp", "Prod", "Real Wage", "Unemp"))

## For SVAR
amat <- diag(4)
diag(amat) <- NA
svar.a <- SVAR(var.2c, estmethod = "direct", Amat = amat)
irf.sa <- irf(svar.a)
ggplot(irf.sa, main="Canada", sub="Structural IRF", 
cap="Caption: The original time series are published by the OECD.",
var_name=c("Emp", "Prod", "Real Wage", "Unemp"), dev_new=TRUE)
}
}
\references{
Victor Espinoza, (2022), 
"Plot() impulse response function - show more than one in one window?,"
stackoverflow. \url{https://stackoverflow.com/questions/68010256/plot-impulse-response-function-show-more-than-one-in-one-window}
}
\author{
Victor Espinoza (stackoverflow), Koichi (Koiti) Yano
}
