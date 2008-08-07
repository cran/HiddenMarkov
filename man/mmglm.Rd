\name{mmglm}
\alias{mmglm}

\title{Markov Modulated GLM Object}

\description{
Creates a Markov modulated generalised linear model object with class \code{"mmglm"}.
}

\usage{
mmglm(x, Pi, delta, family, link, beta, glmformula = formula(y~x1),
      sigma = NA, nonstat = TRUE)
}

\arguments{
  \item{x}{a dataframe containing the observed variable (i.e. the response variable in the generalised linear model) and the covariate. Currently, the response variable must be named \code{y} and the covariate \code{x1}.  Alternatively, \code{x} could be specified as \code{NULL}, meaning that the data will be added later (e.g. simulated). See Details below for the binomial case.}
  \item{Pi}{is the \eqn{m \times m} transition probability matrix of the hidden Markov chain.}
  \item{delta}{is the marginal probability distribution of the \eqn{m} hidden states at the first time point.}
  \item{family}{character string, the GLM family, one of \code{"gaussian"}, \code{"poisson"}, \code{"Gamma"} or \code{"binomial"}.}
  \item{link}{character string, the link function. If \code{family == "binomial"}, then one of \code{"logit"}, \code{"probit"} or \code{"cloglog"}; else one of \code{"identity"}, \code{"inverse"} or \code{"log"}.}
  \item{beta}{a \eqn{2 \times m} matrix containing parameter estimates. The first row contains the \eqn{m} constants in the linear predictor for each Markov state, and the second row contains the linear regression coefficient in the linear predictor for each Markov state.}
  \item{glmformula}{currently the only model formula is \code{y~x1}.}
  \item{sigma}{if \code{family == "gaussian"}, then it is the variance; if \code{family == "Gamma"}, then it is \code{1/sqrt(shape)}; for each Markov state.}
  \item{nonstat}{is logical, \code{TRUE} if the homogeneous Markov chain is assumed to be non-stationary, default.}
}

\value{
A \code{\link[base]{list}} object with class \code{"mmglm"}, containing the above arguments as named components.
}

\details{
This model assumes that the observed responses are ordered in time, together with a covariate at each point. The model is based on a simple regression model within the \code{\link{glm}} framework (see McCullagh \& Nelder, 1989), but where the coefficients \eqn{\beta_0} and \eqn{\beta_1} in the linear predictor vary according to a hidden Markov state. The responses are assumed to be conditionally independent given the value of the Markov chain.

If \code{family == "binomial"} then the response variable \code{y} is interpreted as the number of successes. The dataframe \code{x} must also contain a variable called \code{size} being the number of Bernoulli trials. This is different to the format used by the function \code{\link{glm}} where \code{y} would be a matrix with two columns containing the number of successes and failures, respectively. The different format here allows one to specify the number of Bernoulli trials \emph{only} so that the number of successes or failures can be simulated later.

When the density function of the response variable is from the exponential family (Charnes et al, 1976, Eq. 2.1), the likelihood function (Charnes et al, 1976, Eq. 2.4) can be maximised by using iterative weighted least squares (Charnes et al, 1976, Eq. 1.1 and 1.2). This is the method used by the \R function \code{\link{glm}}. In this Markov modulated version of the model, the third term of the complete data log-likelihood, as given in Harte (2006, \S 2.3), needs to be maximised. This is simply the sum of the individual log-likelihood contributions of the response variable weighted by the Markov state probabilities calculated in the E-step. This can also be maximised using iterative least squares by passing these additional weights (Markov state probabilities) into the \code{\link{glm}} function.
}


\examples{
delta <- c(0,1)

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

#--------------------------------------------------------
#     Poisson with log link function

x <- mmglm(NULL, Pi, delta, family="poisson", link="log",
           beta=rbind(c(0.1, -0.1), c(1, 5)))

x <- simulate(x, nsim=5000, seed=10)

y <- BaumWelch(x)

hist(residuals(y))
print(summary(y))
print(logLik(y))

#--------------------------------------------------------
#     Binomial with logit link function

x <- mmglm(NULL, Pi, delta, family="binomial", link="logit",
           beta=rbind(c(0.1, -0.1), c(1, 5)))

x <- simulate(x, nsim=5000, seed=10)

y <- BaumWelch(x)

hist(residuals(y))
print(summary(y))
print(logLik(y))

#--------------------------------------------------------
#     Gaussian with identity link function

x <- mmglm(NULL, Pi, delta, family="gaussian", link="identity",
           beta=rbind(c(0.1, -0.1), c(1, 5)), sigma=c(1, 2))

x <- simulate(x, nsim=5000, seed=10)

y <- BaumWelch(x)

hist(residuals(y))
print(summary(y))
print(logLik(y))

#--------------------------------------------------------
#     Gamma with log link function

x <- mmglm(NULL, Pi, delta, family="Gamma", link="log",
           beta=rbind(c(2, 1), c(-2, 1.5)), sigma=c(0.2, 0.1))

x1 <- seq(0.01, 0.99, 0.01)
plot(x1, exp(x$beta[1,2] + x$beta[2,2]*x1), type="l",
     xlim=c(0,1),ylim=c(0, 10), col="red", lwd=3)
points(x1, exp(x$beta[1,1] + x$beta[2,1]*x1), type="l",
       col="blue", lwd=3)

x <- simulate(x, nsim=1000, seed=10)

points(x$x$x1, x$x$y)

x$beta[2,] <- c(-3, 4)
y <- BaumWelch(x, bwcontrol(posdiff=FALSE))

hist(residuals(y))
print(summary(y))
print(logLik(y))
}

\keyword{classes}

