\name{probhmm}
\alias{probhmm}

\title{Conditional Distribution Function}

\description{
Calculates the distribution function at each point for a \code{\link{dthmm}} process given the complete observed process except the given point.
}

\usage{
probhmm(x, Pi, delta, distn, pm, pn = NULL, adj = 0, ...)
}

\arguments{
  \item{x}{is a vector of length \eqn{n} containing the observed process.}
  \item{Pi}{is the \eqn{m \times m} transition probability matrix of the hidden Markov chain.}
  \item{delta}{is the marginal probability distribution of the \eqn{m} hidden states at the first time point.}
  \item{distn}{is a character string with the distribution name, e.g. \code{"norm"} or \code{"pois"}. If the distribution is specified as \code{"wxyz"} then a distribution function called \code{"pwxyz"} should be available, in the standard \R\ format (e.g. \code{\link{pnorm}} or \code{\link{ppois}}).}
  \item{pm}{is a list object containing the (Markov dependent) parameter values associated with the distribution of the observed process (see \code{\link{dthmm}}).}
  \item{pn}{is a list object containing the observation dependent parameter values associated with the distribution of the observed process (see \code{\link{dthmm}}).}
  \item{adj}{zero or one, being an adjustment for discrete distributions.}
  \item{...}{other arguments. This allows more complicated objects with the required arguments, but with redundant components also, to be passed to this function.}
}

\details{
Let \eqn{X^{(-i)}} denote the entire process, except with the point \eqn{X_i} removed. The distribution function at the point \eqn{X_i} is \eqn{\Pr\{ X_i \le x_i \,|\, X^{(-i)} = x^{(-i)} \}}. This \R\ function calculates the distribution function for each point \eqn{X_i} for \eqn{i=1, \cdots, n}. This is done by using the forward and backward probabilities before and after the \eqn{i}th point, respectively.

In the programming code, note the subtraction of the mean. This is to stop underflow when the exponential is taken. Removal of the mean is automatically compensated for by the fact that the same factor is removed in both the numerator and denominator.
}

\value{
A vector containing the probability.
}

\references{
Zucchini, W. (2005). \emph{Hidden Markov Models Short Course, 3--4 April 2005.} Macquarie University, Sydney.
}

\seealso{\code{\link{residuals}}}

\keyword{distribution}
