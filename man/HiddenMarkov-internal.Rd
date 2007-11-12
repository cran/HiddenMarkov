\name{HiddenMarkov-internal}
\alias{residuals.mmpp}
\alias{as.dthmm}
\alias{as.mmglm}
\alias{makedensity}
\alias{makedensity1}
\alias{makedistn}
\alias{getj}
\alias{dglm}
\alias{pglm}

\title{Internally Used Functions}

\description{
This page lists internally used functions. They should not be required by most users.
}

\usage{
\method{residuals}{mmpp}(object, ...)

as.dthmm(object)
as.mmglm(object)

makedensity(distn)
makedensity1(distn)
makedistn(distn)

getj(x, j)

dglm(x, x1, beta0, beta1, sigma, family, link, size = NA,
     log = FALSE)
pglm(q, x1, beta0, beta1, sigma, family, link, size = NA,
     log = FALSE)
}

\details{
The function \code{makedensity} is used to reparameterise various \R\ probability (or density) functions (e.g. \code{\link{dnorm}} and \code{\link{dpois}}) into a format with a standard argument list. Similarly, \code{makedistn} reparameterises a distribution function.

The function \code{getj} is used to extract the \eqn{j}th element from each vector component in a list object.

The function \code{as.dthmm} coerces an object with class \code{"\link{mmglm}"} to an object with class \code{"\link{dthmm}"}. Similarly, the function \code{as.mmglm} coerces an object with class \code{"\link{dthmm}"} to an object with class \code{"\link{mmglm}"} (if possible).

The functions \code{dglm} and \code{pglm} calculate the density and probability, respectively, for an observation given a generalised linear model.
}

\keyword{internal}
