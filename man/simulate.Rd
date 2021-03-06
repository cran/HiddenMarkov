\name{simulate}
\alias{simulate}
\alias{simulate.dthmm}
\alias{simulate.mchain}
\alias{simulate.mmglm0}
\alias{simulate.mmglm1}
\alias{simulate.mmglmlong1}
\alias{simulate.mmpp}

\title{Simulate Hidden Markov Process}
\description{
These functions provide methods for the generic function \code{\link[stats]{simulate}}.
}
\usage{
\method{simulate}{dthmm}(object, nsim = 1, seed = NULL, ...)
\method{simulate}{mchain}(object, nsim = 1, seed = NULL, ...)
\method{simulate}{mmglm0}(object, nsim = 1, seed = NULL, ...)
\method{simulate}{mmglm1}(object, nsim = 1, seed = NULL, ...)
\method{simulate}{mmglmlong1}(object, nsim = 1, seed = NULL, ...)
\method{simulate}{mmpp}(object, nsim = 1, seed = NULL, ...)
}

\arguments{
  \item{object}{an object with class \code{"\link{dthmm}"}, \code{"\link{mchain}"}, \code{"\link{mmglm0}"}, \code{"\link{mmglm1}"}, \code{"\link{mmglmlong1}"} or \code{"\link{mmpp}"}.}
  \item{nsim}{number of points to simulate.}
  \item{seed}{seed for the random number generator.}
  \item{\dots}{other arguments.}
}

\details{
Below details about particular methods are given where necessary.
\describe{
\item{\code{simulate.mmglm0}}{If the covariate \code{x1} is \code{NULL}, then uniform (0,1) variables are generated as the values for \code{x1}. When the \code{family} is \code{"binomial"} and \code{size} is \code{NULL} (i.e. the number of Bernoulli trials are not specified), then they are simulated as \code{100+\link{rpois}(nsim, lambda=5)}.}
}

The code for the methods \code{"\link{dthmm}"}, \code{"\link{mchain}"}, \code{"\link{mmglm0}"}, \code{"\link{mmglm1}"},\code{"\link{mmglmlong1}"} and \code{"\link{mmpp}"} can be viewed by appending \code{simulate.dthmm}, \code{simulate.mchain}, \code{simulate.mmglm0}, \code{simulate.mmglm1}, \code{simulate.mmglmlong1}  or \code{simulate.mmpp}, respectively, to \code{HiddenMarkov:::}, on the \R command line; e.g. \code{HiddenMarkov:::dthmm}. The three colons are needed because these method functions are not in the exported NAMESPACE.
}


\value{
The returned object has the same class as the input object and contains the components that were in the input object. Additional components depend on the class as below:

\code{"\link{dthmm}"}: it will also have a vector \code{x} containing the simulated values;

\code{"\link{mmglm0}"}: it will also contain a dataframe \code{x} about the glm; and

\code{"\link{mmpp}"}: \code{tau} contains times of the simulated Poisson events (plus time 0), \code{ys} is a vector of states at the time of each event, \code{y} is the sequence of states visited, and \code{x} is the time spent in each state.
}

\examples{
#    The hidden Markov chain has 5 states with transition matrix:

Pi <- matrix(c(1/2, 1/2,   0,   0,   0,
               1/3, 1/3, 1/3,   0,   0,
                 0, 1/3, 1/3, 1/3,   0,
                 0,   0, 1/3, 1/3, 1/3,
                 0,   0,   0, 1/2, 1/2),
             byrow=TRUE, nrow=5)

#--------------------------------------------
#   simulate a Poisson HMM

x <- dthmm(NULL, Pi, c(0, 1, 0, 0, 0), "pois",
           list(lambda=c(1, 4, 2, 5, 3)), discrete = TRUE)

x <- simulate(x, nsim=2000)

#    check Poisson means
for (i in 1:5) print(mean(x$x[x$y==i]))

#--------------------------------------------
#   simulate a Gaussian HMM

x <- dthmm(NULL, Pi, c(0, 1, 0, 0, 0), "norm",
           list(mean=c(1, 4, 2, 5, 3), sd=c(0.5, 1, 1, 0.5, 0.1)))

x <- simulate(x, nsim=2000)

#    check means and standard deviations
for (i in 1:5) print(mean(x$x[x$y==i]))
for (i in 1:5) print(sd(x$x[x$y==i]))
}

\keyword{methods}
\keyword{datagen}
