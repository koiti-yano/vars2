% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Psi.R
\name{Psi}
\alias{Psi}
\alias{Psi.varest}
\alias{Psi.vec2var}
\title{Coefficient matrices of the orthogonalised MA represention}
\usage{
Psi(x, nstep = 10, ...)
}
\arguments{
\item{x}{An object of class \sQuote{\code{varest}}, generated by
\command{VAR()}, or an object of class \sQuote{\code{vec2var}}, generated by
\command{vec2var()}.}

\item{nstep}{An integer specifying the number of othogonalised moving error
coefficient matrices to be calculated.}

\item{...}{Dots currently not used.}
}
\value{
An array with dimension \eqn{(K \times K \times nstep + 1)} holding
the estimated orthogonalised coefficients of the moving average
representation.
}
\description{
Returns the estimated orthogonalised coefficient matrices of the moving
average representation of a stable VAR(p) as an array.
}
\details{
In case that the components of the error process are instantaneously
correlated with each other, that is: the off-diagonal elements of the
variance-covariance matrix \eqn{\Sigma_u} are not null, the impulses
measured by the \eqn{\Phi_s} matrices, would also reflect disturbances from
the other variables. Therefore, in practice a Choleski decomposition has
been propagated by considering \eqn{\Sigma_u = PP'} and the orthogonalised
shocks \eqn{\bold{\epsilon}_t = P^{-1}\bold{u}_t}. The moving average
representation is then in the form of: \deqn{ \bold{y}_t = \Psi_0
\bold{\epsilon}_t + \Psi_1 \bold{\epsilon}_{t-1} + \Psi
\bold{\epsilon}_{t-2} + \ldots , } whith \eqn{\Psi_0 = P} and the matrices
\eqn{\Psi_s} are computed as \eqn{\Psi_s = \Phi_s P} for \eqn{s = 1, 2, 3,
\ldots}.
}
\note{
The first returned array element is the starting value, \emph{i.e.},
\eqn{\Psi_0}. Due to the utilisation of the Choleski decomposition, the
impulse are now dependent on the ordering of the vector elements in
\eqn{\bold{y}_t}.
}
\examples{

data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
Psi(var.2c, nstep=4)

}
\references{
Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton
University Press, Princeton.

Lütkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
Analysis}, Springer, New York.
}
\seealso{
\code{\link{Phi}}, \code{\link{VAR}}, \code{\link{SVAR}},
\code{\link{vec2var}}
}
\author{
Bernhard Pfaff
}
\keyword{regression}
