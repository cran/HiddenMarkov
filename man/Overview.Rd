\name{Overview}
\alias{Overview}

\title{Overview of Package}

\description{
In this topic we give an overview of the package.
}

\section{Classes of Hidden Markov Models Analysed}{
The classes of models currently fitted by the package are listed below. Each are defined within an object that contains the data, current parameter values, and other model characteristics.

\describe{
\item{Discrete Time Hidden Markov Model:}{
is described under the topic \code{\link{dthmm}}. This model can be simulated or fitted to data by defining the required model structure within an object of class \code{"\link{dthmm}"}.}
\item{Markov Modulated Generalised Linear Model:}{
is described under the topic \code{\link{mmglm}}. This model can be simulated or fitted to data by defining the required model structure within an object of class \code{"\link{mmglm}"}.}
\item{Markov Modulated Poisson Process:}{
is described under the topic \code{\link{mmpp}}. This model can be simulated or fitted to data by defining the required model structure within an object of class \code{"\link{mmpp}"}.}
}
}

\section{Main Tasks Performed by the Package}{
The main tasks performed by the package are listed below. These can be achieved by calling the appropriate generic function.
\describe{
\item{Simulation of HMMs:}{can be performed by the function \code{\link{simulate}}.}
\item{Parameter Estimation:}{can be performed by the function \code{\link{BaumWelch}}.}
\item{Model Residuals:}{can be extracted with the function \code{\link{residuals}}.}
\item{Model Summary:}{can be extracted with the function \code{\link{summary}}.}
\item{Log-Likelihood:}{can be calculated with the function \code{\link{logLik}}.}
\item{Prediction of the Markov States:}{can be performed by the function \code{\link{Viterbi}}.}
}
All other functions in the package are called from within the above generic functions, and only need to be used if their output is specifically required. 
}


\section{Acknowledgement}{
Many of the functions contained in the package are based on those of Walter Zucchini (2005).
}

\references{
Zucchini, W. (2005). \emph{Hidden Markov Models Short Course, 3--4 April 2005.} Macquarie University, Sydney.
}

\keyword{documentation}
