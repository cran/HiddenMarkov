\name{residuals}
\alias{residuals}
\alias{residuals.dthmm}
\alias{residuals.mmglm}

\title{Residuals of Hidden Markov Models}

\description{
Provides methods for the generic function \code{\link[stats]{residuals}}. There is currently no method for objects of class \code{"\link{mmpp}"}.
}

\usage{
\method{residuals}{dthmm}(object, ...)
\method{residuals}{mmglm}(object, ...)
}

\arguments{
  \item{object}{an object with class \code{\link{dthmm}} or \code{\link{mmglm}}. There is not yet a method for \code{\link{mmpp}}.}
  \item{...}{other arguments.}
}

\details{
For objects of class \code{"\link{dthmm}"} or \code{"\link{mmglm}"} the calculated residuals are pseudo residuals. Under satisfactory conditions they have an approximate standard normal distribution.

Initially the function \code{\link{probhmm}} is called. If the model fits satisfactorily, the returned values should be approximately uniformly distributed. Hence by applying the function \code{\link[stats:Normal]{qnorm}}, the resultant \dQuote{residuals} should have an approximate standard normal distribution.

When the distribution (observed) is discrete an adjustment is made. However, if relatively few of the possible outcomes are observed, the pseudo residuals will be rather poorly described by the standard normal distribution; see the Poisson example below.

The code for the methods \code{"\link{dthmm}"} and \code{"\link{mmglm}"} can be viewed by typing \code{residuals.dthmm} or \code{residuals.mmglm}, respectively, on the \R command line.
}

\value{
A vector containing the pseudo residuals.
}

\examples{
#   Example Using Beta Distribution

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

n <- 2000

x <- dthmm(NULL, Pi, c(0,1), "beta",
           list(shape1=c(2, 6), shape2=c(6, 2)))

x <- simulate(x, nsim=n, seed=5)

y <- residuals(x) 

w <- hist(y, main="Beta HMM: Pseudo Residuals")
z <- seq(-3, 3, 0.01)
points(z, dnorm(z)*n*(w$breaks[2]-w$breaks[1]), col="red", type="l")
box()

qqnorm(y, main="Beta HMM: Q-Q Plot of Pseudo Residuals")
abline(a=0, b=1, lty=3)
abline(h=seq(-2, 2, 1), lty=3)
abline(v=seq(-2, 2, 1), lty=3)


#-----------------------------------------------
#   Example Using Gaussian Distribution

Pi <- matrix(c(1/2, 1/2,   0,   0,   0,
               1/3, 1/3, 1/3,   0,   0,
                 0, 1/3, 1/3, 1/3,   0,
                 0,   0, 1/3, 1/3, 1/3,
                 0,   0,   0, 1/2, 1/2),
             byrow=TRUE, nrow=5)

x <- dthmm(NULL, Pi, c(0, 1, 0, 0, 0), "norm",
           list(mean=c(1, 4, 2, 5, 3), sd=c(0.5, 1, 1, 0.5, 0.1)))

n <- 2000
x <- simulate(x, nsim=n, seed=5)

y <- residuals(x) 

w <- hist(y, main="Gaussian HMM: Pseudo Residuals")
z <- seq(-3, 3, 0.01)
points(z, dnorm(z)*n*(w$breaks[2]-w$breaks[1]), col="red", type="l")
box()

qqnorm(y, main="Gaussian HMM: Q-Q Plot of Pseudo Residuals")
abline(a=0, b=1, lty=3)
abline(h=seq(-2, 2, 1), lty=3)
abline(v=seq(-2, 2, 1), lty=3)


#-----------------------------------------------
#   Example Using Poisson Distribution  

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

x <- dthmm(NULL, Pi, c(0, 1), "pois",
           list(lambda=c(1, 5)), discrete=TRUE)

n <- 2000
x <- simulate(x, nsim=n, seed=5)

y <- residuals(x) 

w <- hist(y, main="Poisson HMM: Pseudo Residuals")
z <- seq(-3, 3, 0.01)
points(z, dnorm(z)*n*(w$breaks[2]-w$breaks[1]), col="red", type="l")
box()

qqnorm(y, main="Poisson HMM: Q-Q Plot of Pseudo Residuals")
abline(a=0, b=1, lty=3)
abline(h=seq(-2, 2, 1), lty=3)
abline(v=seq(-2, 2, 1), lty=3)
}

\keyword{methods}

