\name{Mstep}
\alias{Mstep}
\alias{Mstep.beta}
\alias{Mstep.binom}
\alias{Mstep.exp}
\alias{Mstep.gamma}
\alias{Mstep.glm}
\alias{Mstep.lnorm}
\alias{Mstep.logis}
\alias{Mstep.norm}
\alias{Mstep.pois}

\title{M Step of EM Algorithm}

\description{
Performs the \emph{maximisation} step of the EM algorithm for a \code{\link{dthmm}} process. This function is called by the \code{\link{BaumWelch}} function. The Baum-Welch algorithm used in the HMM literature is a version of the EM algorithm.
}
\usage{
Mstep.beta(x, cond, pm, pn, maxiter = 200)
Mstep.binom(x, cond, pm, pn)
Mstep.exp(x, cond, pm, pn)
Mstep.gamma(x, cond, pm, pn, maxiter = 200)
Mstep.glm(x, cond, pm, pn, family, link)
Mstep.lnorm(x, cond, pm, pn)
Mstep.logis(x, cond, pm, pn, maxiter = 200)
Mstep.norm(x, cond, pm, pn)
Mstep.pois(x, cond, pm, pn)
}

\arguments{
  \item{x}{is a vector of length \eqn{n} containing the observed process.}
  \item{cond}{is an object created by \code{\link{Estep}}.}
  \item{family}{character string, the GLM family, one "gaussian", "poisson", "Gamma" or "binomial".}
  \item{link}{character string, the link function. If \code{family == "Binomial"}, then one of "logit", "probit" or "cloglog"; else one of "identity", "inverse" or "log".}
  \item{pm}{is a list object containing the current (Markov dependent) parameter estimates associated with the distribution of the observed process (see \code{\link{dthmm}}). These are only used as initial values if the algorithm within the \code{Mstep} is iterative.}
  \item{pn}{is a list object containing the observation dependent parameter values associated with the distribution of the observed process (see \code{\link{dthmm}}).}
  \item{maxiter}{maximum number of Newton-Raphson iterations.}
}

\details{
The functions \code{Mstep.beta}, \code{Mstep.binom}, \code{Mstep.exp}, \code{Mstep.gamma}, \code{Mstep.lnorm}, \code{Mstep.logis}, \code{Mstep.norm} and \code{Mstep.pois} perform the maximisation step for the Beta, Binomial, Exponential, Gamma, Log Normal, Logistic, Normal and Poisson distributions, respectively. Each function has the same argument list, even if specific arguments are redundant, because the functions are called from within other functions in a generic like manner. Specific notes for some follow.

\describe{
\item{\code{Mstep.beta}}{The \R\ functions for the Beta distribution have arguments \code{shape1} and \code{shape2}; and the density also has \code{ncp}. We only use \code{shape1} and \code{shape2}, i.e. \code{ncp} is assumed to be zero. Different combinations of \code{"shape1"} and \code{"shape2"} can be \dQuote{time} dependent (specified in \code{pn}) and Markov dependent (specified in \code{pm}). However, each should only be specified in one (see topic \code{\link{dthmm}}).
}

\item{\code{Mstep.binomial}}{The \code{size} argument of the binomial distribution should always be specified in the \code{pn} argument (see topic \code{\link{dthmm}}).
}

\item{\code{Mstep.gamma}}{The \R\ functions for the Gamma distribution have arguments \code{shape}, \code{rate} and \code{scale}. Since \code{scale} is redundant, we only use \code{shape} and \code{rate}. Different combinations of \code{"shape"} and \code{"rate"} can be \dQuote{time} dependent (specified in \code{pn}) and Markov dependent (specified in \code{pm}). However, each should only be specified in one (see topic \code{\link{dthmm}}).
}

\item{\code{Mstep.lnorm}}{Different combinations of \code{"meanlog"} and \code{"sdlog"} can be \dQuote{time} dependent (specified in \code{pn}) and Markov dependent (specified in \code{pm}). However, each should only be specified in one (see topic \code{\link{dthmm}}).
}

\item{\code{Mstep.logis}}{Different combinations of \code{"location"} and \code{"scale"} can be \dQuote{time} dependent (specified in \code{pn}) and Markov dependent (specified in \code{pm}). However, each should only be specified in one (see topic \code{\link{dthmm}}).
}

\item{\code{Mstep.norm}}{Different combinations of \code{"mean"} and \code{"sd"} can be \dQuote{time} dependent (specified in \code{pn}) and Markov dependent (specified in \code{pm}). However, each should only be specified in one (see topic \code{\link{dthmm}}).
}

}
}

\section{Modifications}{
Consider a distribution with two parameters where both parameters are Markov dependent, but one is known and the other requires estimation. For example, consider the Gaussian distribution. Say we know the Markov dependent means, but we need to estimate the standard deviations. Since both parameters are Markov dependent, they both need to be specified in the \code{pm} argument. The estimation of the distribution specific parameters takes place in the Mstep, in this case \code{\link{Mstep.norm}}. To achieve what we want, we need to modify this function. In this case it is relatively easy (see code in \dQuote{Examples} below. From the function \code{\link{Mstep.norm}}, take the code under the section \code{if (all(nms==c("mean", "sd")))}, i.e. both of the parameters are Markov dependent. However, replace the line where the mean is estimated to \code{mean <- pm$mean}, i.e. leave it as was initially specified. Then \code{\link{source}} this revised function so that is found by \R\ in preference to the standard version in the package.

One needs to take a little more care when dealing with a distributions like the beta, where the cross derivatives of the log likelihood between the parameters, i.e. \eqn{\partial^2 \log L /(\partial \alpha_1 \partial \alpha_2)} are non-zero.
}



\value{
A list object with the same structure as \code{pm} (see topic \code{\link{dthmm}}).
}

\seealso{
\code{\link{BaumWelch}}, \code{\link{Estep}}
}

\section{Note}{
The Mstep functions can be used to estimate the maximum likelihood parameters from a simple sample. See the example below where this is done for the logistic distribution.
}

\examples{
#    Fit logistic distribution to a simple single sample

#    Simulate data
n <- 20000
location <- -2
scale <- 1.5

x <- rlogis(n, location, scale)

#    give each datum equal weight
cond <- NULL
cond$u <- matrix(rep(1/n, n), ncol=1)

#    calculate maximum likelihood parameter estimates
#    start iterations at values used to simulate
print(Mstep.logis(x, cond,
                  pm=list(location=location,
                          scale=scale)))


#   Example with Gaussian Observations
#   Assume that both mean and sd are Markov dependent, but the means
#   are known and sd requires estimation (See "Modifications" above).

Mstep.norm <- function(x, cond, pm, pn){
    nms <- sort(names(pm))
    n <- length(x)
    m <- ncol(cond$u)
    if (all(nms==c("mean", "sd"))){
        mean <- pm$mean
        sd <- sqrt(apply((matrix(x, nrow = n, ncol=m) - 
                   matrix(mean,
                   nrow = n, ncol=m, byrow=TRUE))^2 * cond$u, MARGIN=2,
                   FUN=sum)/apply(cond$u, MARGIN = 2, FUN = sum))
        return(list(mean=mean, sd=sd))
    }
}

Pi <- matrix(c(1/2, 1/2,   0,
               1/3, 1/3, 1/3,
                 0, 1/2, 1/2),
             byrow=TRUE, nrow=3)
p1 <- c(1, 6, 3)
p2 <- c(0.5, 1, 0.5)
n <- 1000

pm <- list(mean=p1, sd=p2)

x <- dthmm(NULL, Pi, c(0, 1, 0), "norm", pm)

x <- simulate(x, n)

#    use above parameter values as initial values
y <- BaumWelch(x)

print(y$delta)
print(y$pm)
print(y$Pi)
}

\keyword{optimize}

