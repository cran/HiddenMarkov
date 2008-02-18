\name{BaumWelch}
\alias{BaumWelch}
\alias{BaumWelch.dthmm}
\alias{BaumWelch.mmglm}
\alias{BaumWelch.mmpp}

\title{Estimate Parameters Using Baum-Welch Algorithm}

\description{
Estimates the parameters of a hidden Markov model. The Baum-Welch algorithm (Baum et al, 1970) referred to in the HMM literature is a version of the EM algorithm (Dempster et al, 1977). See Hartley (1958) for an earlier application of the EM methodology, though not referred to as such.}

\usage{
BaumWelch(object, control, ...)
\method{BaumWelch}{dthmm}(object, control = bwcontrol(), ...)
\method{BaumWelch}{mmglm}(object, control = bwcontrol(), ...)
\method{BaumWelch}{mmpp}(object, control = bwcontrol(), ...)
}

\arguments{
  \item{object}{an object of class \code{"\link{dthmm}"}, \code{"\link{mmglm}"}, or \code{"\link{mmpp}"}.}
  \item{control}{a list of control settings for the iterative process. These can be changed by using the function \code{\link{bwcontrol}}.}
  \item{\dots}{other arguments.}
}

\details{
The initial parameter values used by the EM algorithm are those that are contained within the input \code{object}.

The code for the methods \code{"\link{dthmm}"}, \code{"\link{mmglm}"} and \code{"\link{mmpp}"} can be viewed by typing \code{BaumWelch.dthmm}, \code{BaumWelch.mmglm} or \code{BaumWelch.mmpp}, respectively, on the \R command line.
}

\value{
The output object (a \code{\link[base]{list}}) with have the same class as the input, and will have the same components. The parameter values will be replaced by those estimated by this function. The object will also contain additional components. 

An object of class \code{"\link{dthmm}"} will also contain
  \item{u}{an \eqn{n \times m} matrix containing estimates of the conditional expectations. See \dQuote{Details} in \code{\link{Estep}}.}
  \item{v}{an \eqn{n \times m \times m} array containing estimates of the conditional expectations. See \dQuote{Details} in \code{\link{Estep}}.}
  \item{LL}{value of log-likelihood at the end.}
  \item{iter}{number of iterations performed.}
  \item{diff}{difference between final and previous log-likelihood.}
}

\seealso{
\code{\link{logLik}}, \code{\link{residuals}}, \code{\link{simulate}}, \code{\link{summary}}
, \code{\link{neglogLik}}}

\references{
Baum, L.E.; Petrie, T.; Soules, G. \& Weiss, N. (1970). A maximization technique occurring in the statistical analysis of probabilistic functions of Markov chains. \emph{Annals of Mathematical Statistics} \bold{41(1)}, 164--171. DOI: \url{http://dx.doi.org/10.1214/aoms/1177697196}

Dempster, A.P.; Laird, N.M. \& Rubin, D.B. (1977). Maximum likelihood from incomplete data via the EM algorithm (with discussion). \emph{J. Royal Statist. Society B} \bold{39(1)}, 1--38.

Hartley, H.O. (1958). Maximum likelihood estimation from incomplete data. \emph{Biometrics} \bold{14(2)}, 174--194. DOI: \url{http://dx.doi.org/10.2307/2527783}
}

\keyword{methods}
\keyword{optimize}
