\name{dthmm}
\alias{dthmm}

\title{Discrete Time HMM Object}

\description{
Creates a discrete time hidden Markov model object with class \code{"dthmm"}.
}

\usage{
dthmm(x, Pi, delta, distn, pm, pn = NULL, discrete = FALSE,
      nonstat = TRUE)
}

\arguments{
  \item{x}{is a vector of length \eqn{n} containing the observed process. Alternatively, \code{x} could be specified as \code{NULL}, meaning that the data will be added later (e.g. simulated).}
  \item{Pi}{is the \eqn{m \times m} transition probability matrix of the homogeneous hidden Markov chain.}
  \item{delta}{is the marginal probability distribution of the \eqn{m} hidden states at the first time point.}
  \item{distn}{is a character string with the distribution name, e.g. \code{"norm"} or \code{"pois"}. If the distribution is specified as \code{"wxyz"} then a distribution function called \code{"pwxyz"} should be available, in the standard \R\ format (e.g. \code{\link{pnorm}} or \code{\link{ppois}}).}
  \item{pm}{is a list object containing the (Markov dependent) parameter values associated with the distribution of the observed process (see below).}
  \item{pn}{is a list object containing the observation dependent parameter values associated with the distribution of the observed process (see below).}
  \item{discrete}{is logical, and is \code{TRUE} if \code{distn} is a discrete distribution.}
  \item{nonstat}{is logical, \code{TRUE} if the homogeneous Markov chain is assumed to be non-stationary, default. See \dQuote{Details} below.}
}

\value{
A \code{\link[base]{list}} object with class \code{"dthmm"}, containing the above arguments as named components.
}

\section{Notation}{
\enumerate{
\item MacDonald \& Zucchini (1997) use \eqn{t} to denote the \emph{time}, where \eqn{t = 1, \cdots, T}. To avoid confusion with other uses of \code{\link[base]{t}} and \code{\link[base:logical]{T}} in \R\, we use \eqn{i = 1, \cdots, n}.

\item We denote the observed sequence as \eqn{\{X_i\},\ i = 1, \cdots, n}; and the hidden Markov chain as \eqn{\{C_i\},\ i = 1, \cdots, n}.

\item The history of the observed process up to time \eqn{i} is denoted by \eqn{X^{(i)}}, i.e.
\deqn{
X^{(i)} = (X_1, \cdots, X_i)
}
where \eqn{i = 1, \cdots, n}. Similarly for \eqn{C^{(i)}}.


\item The hidden Markov chain has \eqn{m} states denoted by \eqn{1, \cdots, m}.

\item The Markov chain transition probability matrix is denoted by \eqn{\Pi}, where the \eqn{(j, k)}th element is
\deqn{
\pi_{jk} = \Pr\{ C_{i+1}=k \, | \, C_i=j \}
}
for all \eqn{i} (i.e. all time points), and \eqn{j,k = 1, \cdots, m}.

\item The Markov chain is assumed to be \emph{homogeneous}, i.e. for each \eqn{j} and \eqn{k}, \eqn{\pi_{jk}} is constant over time.

\item The Markov chain is said to be \emph{stationary} if the marginal distribution is the same over time, i.e. for each \eqn{j}, \eqn{\delta_j = \Pr\{ C_i = j \}} is constant for all \eqn{i}. The marginal distribution is denoted by \eqn{\delta = (\delta_1, \cdots, \delta_m)}.
}}

\section{List Object pm}{
The list object \code{pm} contains parameter values for the probability distribution of the observed process that are dependent on the hidden Markov state. These parameters are generally required to be estimated. See \dQuote{Modifications} in topic \code{\link{Mstep}} when some do not require estimation.

Assume that the hidden Markov chain has \eqn{m} states, and that there are \eqn{\ell} parameters that are dependent on the hidden Markov state. Then the list object \code{pm} should contain \eqn{\ell} \emph{named} vector components each of length \eqn{m}. The names are determined by the required probability distribution.

For example, if \code{distn == "norm"}, the arguments names must coincide with those used by the functions \code{\link{dnorm}} or \code{\link{rnorm}}, which are \code{mean} and \code{sd}. Each must be specified in either \code{pm} or \code{pn}. If they both vary according to the hidden Markov state then \code{pm} should have the \emph{named} components \code{mean} and \code{sd}. These are both vectors of length \eqn{m} containing the means and standard deviations of the observed process when the hidden Markov chain is in each of the \eqn{m} states. If, for example, \code{sd} was \dQuote{time} dependent, then \code{sd} would be contained in \code{pn} (see below).

If \code{distn == "pois"}, then \code{pm} should have one component named \code{lambda}, being the parameter name in the function \code{\link{dpois}}. Even if there is only one parameter, the vector component should still be within a list and named.
}

\section{List Object pn}{
The list object \code{pn} contains parameter values of the probability distribution for the observed process that are dependent on the observation number or \dQuote{time}. These parameters are assumed to be \emph{known}.

Assume that the observed process is of length \eqn{n}, and that there are \eqn{\ell} parameters that are dependent on the observation number or time. Then the list object \code{pn} should contain \eqn{\ell} \emph{named} vector components each of length \eqn{n}. The names, as in \code{pm}, are determined by the required probability distribution.

For example, in the observed process we may count the number of successes in a \emph{known} number of Bernoulli trials, i.e. the number of Bernoulli trials is known at each time point, but the probability of success varies according to a hidden Markov state. The \code{prob} parameter of \code{\link{rbinom}} (or \code{\link{dbinom}}) would be specified in \code{pm} and the \code{size} parameter would specified in \code{pn}.

One could also have a situation where the observed process was Gaussian, with the means varying according to the hidden Markov state, but the variances varying non-randomly according to the observation number (or vice versa). Here \code{mean} would be specified within \code{pm} and \code{sd} within \code{pn}. Note that a given parameter can only occur within \emph{one} of \code{pm} or \code{pn}.
}

\section{Complete Data Likelihood}{
The \dQuote{complete data likelihood}, \eqn{L_c}, is
\deqn{
L_c = \Pr\{ X_1=x_1, \cdots, X_n=x_n, C_1=c_1, \cdots, C_n=c_n \}\,.
}
This can be shown to be
\deqn{
\Pr\{ X_1=x_1 \,|\, C_1=c_1 \} \Pr\{ C_1=c_1 \} \prod_{i=2}^n \Pr\{ X_i=x_i \,|\, C_i=c_i \} \Pr\{ C_i=c_i \,|\, C_{i-1}=c_{i-1} \}\,,
}
and hence, substituting model parameters, we get
\deqn{
L_c = \delta_{c_1} \pi_{c_1c_2} \pi_{c_2c_3} \cdots \pi_{c_{n-1}c_n} \prod_{i=1}^n \Pr\{ X_i=x_i \,|\, C_i=c_i \}\,,
}
and so
\deqn{
\log L_c = \log \delta_{c_1} +  \sum_{i=2}^n \log \pi_{c_{i-1}c_i} + \sum_{i=1}^n \log \Pr\{ X_i=x_i \,|\, C_i=c_i \}\,.
}
Hence the \dQuote{complete data likelihood} is split into three terms: the first relates to parameters of the marginal distribution (Markov chain), the second to the transition probabilities, and the third to the distribution parameters of the observed random variable. When the Markov chain is non-stationary, each term can be maximised separately.

When the hidden Markov chain is assumed to be stationary, \eqn{\delta = \Pi^\prime \delta} (see topic \code{\link{compdelta}}), and then the first two terms of \eqn{\log L_c} determine the transition probabilities \eqn{\Pi}. This raises more complicated numerical problems, as the first term is effectively a constraint. We deal with this in a slightly ad-hoc manner by effectively disregarding the first term, which is assumed to be relatively small. In the M-step, the transition matrix is determined by the second term, then \eqn{\delta} is estimated using the relation \eqn{\delta = \delta \Pi}.
}

\references{
Elliott, R.J.; Aggoun, L. & Moore, J.B. (1994). \emph{Hidden Markov Models: Estimation and Control.} Springer-Verlag, New York.

Harte, D. (2006). \emph{Mathematical Background Notes for Package \dQuote{HiddenMarkov}.} Statistics Research Associates, Wellington. URL: \url{http://homepages.paradise.net.nz/david.harte/SSLib/Manuals/notes.pdf}.

MacDonald, I.L. & Zucchini, W. (1997). \emph{Hidden Markov and Other Models for Discrete-valued Time Series.} Chapman and Hall/CRC, Boca Raton.

Rabiner, L.R. (1989). A tutorial on hidden Markov models and selected applications in speech recognition. \emph{Proceedings of the IEEE} \bold{77(2)}, 257--286. URL: \url{http://dx.doi.org/10.1109/5.18626}.

Zucchini, W. (2005). \emph{Hidden Markov Models Short Course, 3--4 April 2005.} Macquarie University, Sydney.
}

\examples{
#-----  Test Gaussian Distribution -----

Pi <- matrix(c(1/2, 1/2,   0,
               1/3, 1/3, 1/3,
                 0, 1/2, 1/2),
             byrow=TRUE, nrow=3)

delta <- c(0, 1, 0)

x <- dthmm(NULL, Pi, delta, "norm",
           list(mean=c(1, 6, 3), sd=c(0.5, 1, 0.5)))

x <- simulate(x, nsim=1000)

#    use above parameter values as initial values
y <- BaumWelch(x)

print(summary(y))
print(logLik(y))
hist(residuals(y))

#   check parameter estimates
print(sum(y$delta))
print(y$Pi \%*\% rep(1, ncol(y$Pi)))


#-----  Test Poisson Distribution  -----

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

delta <- c(0, 1)

x <- dthmm(NULL, Pi, delta, "pois", list(lambda=c(4, 0.1)),
           discrete = TRUE)

x <- simulate(x, nsim=1000)

#    use above parameter values as initial values
y <- BaumWelch(x)

print(summary(y))
print(logLik(y))
hist(residuals(y))

#   check parameter estimates
print(sum(y$delta))
print(y$Pi \%*\% rep(1, ncol(y$Pi)))


#-----  Test Exponential Distribution  -----

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

delta <- c(0, 1)

x <- dthmm(NULL, Pi, delta, "exp", list(rate=c(2, 0.1)))

x <- simulate(x, nsim=1000)

#    use above parameter values as initial values
y <- BaumWelch(x)

print(summary(y))
print(logLik(y))
hist(residuals(y))

#   check parameter estimates
print(sum(y$delta))
print(y$Pi \%*\% rep(1, ncol(y$Pi)))


#-----  Test Beta Distribution  -----

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

delta <- c(0, 1)

x <- dthmm(NULL, Pi, delta, "beta", list(shape1=c(2, 6), shape2=c(6, 2)))

x <- simulate(x, nsim=1000)

#    use above parameter values as initial values
y <- BaumWelch(x)

print(summary(y))
print(logLik(y))
hist(residuals(y))

#   check parameter estimates
print(sum(y$delta))
print(y$Pi \%*\% rep(1, ncol(y$Pi)))


#-----  Test Binomial Distribution  -----

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

delta <- c(0, 1)

#   vector of "fixed & known" number of Bernoulli trials
pn <- list(size=rpois(1000, 10)+1)

x <- dthmm(NULL, Pi, delta, "binom", list(prob=c(0.2, 0.8)), pn,
           discrete=TRUE)

x <- simulate(x, nsim=1000)

#    use above parameter values as initial values
y <- BaumWelch(x)

print(summary(y))
print(logLik(y))
hist(residuals(y))

#   check parameter estimates
print(sum(y$delta))
print(y$Pi \%*\% rep(1, ncol(y$Pi)))


#-----  Test Gamma Distribution  -----

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

delta <- c(0, 1)

pm <- list(rate=c(4, 0.5), shape=c(3, 3))

x <- seq(0.01, 10, 0.01)
plot(x, dgamma(x, rate=pm$rate[1], shape=pm$shape[1]),
     type="l", col="blue", ylab="Density")
points(x, dgamma(x, rate=pm$rate[2], shape=pm$shape[2]),
       type="l", col="red")

x <- dthmm(NULL, Pi, delta, "gamma", pm)

x <- simulate(x, nsim=1000)

#    use above parameter values as initial values
y <- BaumWelch(x)

print(summary(y))
print(logLik(y))
hist(residuals(y))

#   check parameter estimates
print(sum(y$delta))
print(y$Pi \%*\% rep(1, ncol(y$Pi)))
}

\keyword{classes}

