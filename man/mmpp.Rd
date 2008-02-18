\name{mmpp}
\alias{mmpp}

\title{Markov Modulated Poisson Process Object}

\description{
Creates a Markov modulated Poisson process model object with class \code{"mmpp"}.
}

\usage{
mmpp(tau, Q, delta, lambda, nonstat = TRUE)
}

\arguments{
  \item{tau}{vector containing the \emph{event times}. Note that the first event is at time zero. Alternatively, \code{tau} could be specified as \code{NULL}, meaning that the data will be added later (e.g. simulated).}
  \item{Q}{the infinitesimal generator matrix of the Markov process.}
  \item{delta}{is the marginal probability distribution of the \eqn{m} hidden states at time zero.}
  \item{lambda}{a vector containing the Poisson rates.}
  \item{nonstat}{is logical, \code{TRUE} if the homogeneous Markov process is assumed to be non-stationary, default.}
}

\value{
A \code{\link[base]{list}} object with class \code{"mmpp"}, containing the above arguments as named components.
}

\details{
The Markov modulated Poisson process is based on a hidden Markov process in continuous time. The initial state probabilities (at time zero) are specified by \code{delta} and the transition rates by the \code{Q} matrix. The rate parameter of the Poisson process (\code{lambda}) is determined by the current state of the hidden Markov process. Within each state, the Poisson process is homogeneous (constant rate parameter). A Poisson event is assumed to occur at time zero and at the end of the observation period, however, state transitions of the Markov process do not necessarily coincide with Poisson events. For more details, see Ryden (1996).
}

\references{
Klemm, A.; Lindemann, C. \& Lohmann, M. (2003). Modeling IP traffic using the batch Markovian arrival process. \emph{Performance Evaluation} \bold{54(2)}, 149--173. DOI: \url{http://dx.doi.org/10.1016/S0166-5316(03)00067-1}

Roberts, W.J.J.; Ephraim, Y. \& Dieguez, E. (2006). On Ryden's EM algorithm for estimating MMPPs. \emph{IEEE Signal Processing Letters} \bold{13(6)}, 373--376. DOI: \url{http://dx.doi.org/10.1109/LSP.2006.871709}

Ryden, T. (1994). Parameter estimation for Markov modulated Poisson processes. \emph{Stochastic Models} \bold{10(4)}, 795--829. DOI: \url{http://dx.doi.org/10.1080/15326349408807323}

Ryden, T. (1996). An EM algorithm for estimation in Markov-modulated Poisson processes. \emph{Computational Statistics & Data Analysis} \bold{21(4)}, 431--447. DOI: \url{http://dx.doi.org/10.1016/0167-9473(95)00025-9}
}

\examples{
Q <- matrix(c(-2,  2,
               1, -1),
            byrow=TRUE, nrow=2)/10

#    NULL indicates that we have no data at this point
x <- mmpp(NULL, Q, delta=c(0, 1), lambda=c(5, 1))

x <- simulate(x, nsim=5000, seed=5)

y <- BaumWelch(x)

print(summary(y))

#    log-likelihood using initial parameter values
print(logLik(x))

#    log-likelihood using estimated parameter values
print(logLik(y))
}

\keyword{classes}

