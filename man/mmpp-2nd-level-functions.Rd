\name{mmpp-2nd-level-functions}
\alias{mmpp-2nd-level-functions}
\alias{forwardback.mmpp}
\alias{Estep.mmpp}

\title{Markov Modulated Poisson Process - 2nd Level Functions}

\description{
These functions have not been put into a generic format, but are called by generic functions.
}

\usage{
forwardback.mmpp(tau, Q, delta, lambda, fortran = TRUE)
Estep.mmpp(tau, Q, delta, lambda, fortran = TRUE)
}

\arguments{
  \item{tau}{vector containing the interevent times. Note that the first event is at time zero.}
  \item{Q}{the infinitesimal generator matrix of the Markov process.}
  \item{lambda}{a vector containing the Poisson rates.}
  \item{delta}{is the marginal probability distribution of the \eqn{m} hidden states at time zero.}
  \item{fortran}{logical, if \code{TRUE} (default) use the Fortran code, else use the \R\ code.}
}

\details{
These functions use the algorithm given by Ryden (1996) based on eigenvalue decompositions.
}

\keyword{misc}

