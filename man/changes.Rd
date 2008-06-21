\name{Changes}
\alias{Changes}
\title{Changes Made to Package HiddenMarkov}
\description{
This page contains a listing of recent changes made to the package.
}

\details{
\enumerate{
\item Since we have included different classes of HMMs (see \code{\link{dthmm}}, \code{\link{mmglm}} and \code{\link{mmpp}}), it is much tidier to use an object orientated approach. This ensures that the functions across all models follow a more consistent naming convention, and also the argument list for the different model functions are more simple and consistent (see \code{\link{HiddenMarkov}}). (14 Sep 2007)

\item The main tasks (model fitting, residuals, simulation, Viterbi, etc) can now be called by generic functions (see topic \code{\link{HiddenMarkov}}). The package documentation has been rearranged so that these generic functions contain the documentation for all model types (e.g. see \code{\link{BaumWelch}}). (14 Sep 2007)

\item There are a number of functions, still contained in the package, that are obsolete. This is either because they do not easily fit into the current naming convention used to implement the more object orientated approach, or their argument list is slightly complicated. These functions have been grouped in the topics \code{\link{dthmm.obsolete}} and \code{\link{mmpp.obsolete}}. (14 Sep 2007)

\item There are various \emph{second level} functions. For example, the model fitting is achieved by the generic \code{\link{BaumWelch}} function. However, this will call functions to do the E-step, M-step, forward and backward probabilities, and so on. At the moment, these \emph{second level} functions have not been modified into an object orientated approach. It is not clear at this point whether this would be advantageous. If one went down this route, then one would probably group all of the E-step functions (for all models) under the same topic. If not, then it may be best to group all second level functions for each model under the same topic (e.g. \code{\link{forwardback}}, \code{\link{probhmm}} and \code{\link{Estep}} would be grouped together, being the second level functions for the \code{\link{dthmm}} model). (14 Sep 2007)

\item The original function called \code{Viterbi} has been renamed to \code{\link{Viterbihmm}}, and \code{\link{Viterbi}} is now a generic function. (14 Sep 2007)

\item Programming code that uses old versions of the functions should still work with this revised version of the package. However, you will get warning messages stating that certain functions are deprecated, and suggesting a possible alternative. To get a quick overview of the programming style, have a look at the examples in topic \code{\link{dthmm}}. (09 Nov 2007)

\item \code{\link{forwardback}}: \code{for} loops replaced by Fortran code; much faster. The corresponding R code is still contained within the function in case the Fortran has incompatibility issues. (23 Nov 2007)

\item \code{\link{forwardback.mmpp}}: \code{for} loops replaced by Fortran code. The corresponding R code is still contained within the function in case the Fortran has incompatibility issues. (24 Nov 2007)

\item \code{\link{Estep.mmpp}}: \code{for} loops replaced by Fortran code. Cuts time considerably. These loops in R used far more time than the forward and backward equations. The corresponding R code is still contained within the function in case the Fortran has incompatibility issues. (27 Nov 2007)

\item \code{\link{forwardback.mmpp}}, \code{\link{forwardback}} and \code{\link{Estep.mmpp}}: argument \code{fortran} added. (3 Dec 2007)

\item \code{\link{forwardback}}, \code{\link{forwardback.mmpp}} and \code{\link{Estep.mmpp}}: inclusion of all variable sized arrays in the Fortran subroutine call to be compatible with non gfortran compilers (3 Dec 2007); more added for calls to Fortran subroutines \code{multi1} and \code{multi2}. (6 Dec 2007)

\item \code{\link{Estep.mmpp}}: error in Fortran code of loop 6; \code{j1=0} to \code{j1=1}. (5 Dec 2007)

\item \code{\link{BaumWelch.mmpp}}: \code{if (diff < 0) stop ...} to \code{if (diff < 0 & control$posdiff) stop ...}, consistent with \code{\link{BaumWelch.dthmm}}. (11 Dec 2007)

\item \code{\link{logLik.dthmm}}, \code{\link{logLik.mmglm}}, \code{\link{logLik.mmpp}}: \code{for} loop replaced by Fortran code. (15 Feb 2008)

\item \code{\link{dthmm}}: argument \code{discrete} set automatically for known distributions, stops if not set for unknown distributions. (15 Feb 2008)

\item \code{\link{neglogLik}}, \code{\link{Pi2vector}}, \code{\link{vector2Pi}}, \code{\link{Q2vector}}, \code{\link{vector2Q}}: new functions providing an alternative means of calculating maximum likelihood parameter estimates. (18 Feb 2008)

\item \code{\link{dthmm}}: argument \code{nonstat} was not set correctly. (21 Jun 2008)
}}


\section{Future Development}{
\enumerate{
\item The functions \code{\link{residuals}} and \code{\link{Viterbi}} need methods for objects with class \code{\link{mmpp}}.

\item A number of the original functions have names that are too general. For example \code{\link{forwardback}} calculates the forward-backward probabilities, but only for the model \code{\link{dthmm}}. The corresponding function for the \code{\link{mmpp}} model is \code{\link{forwardback.mmpp}}. It would be more consistent to attach to these original functions a \code{dthmm} suffix.

\item The demonstration examples are all for \code{\link{dthmm}}. Also need some for \code{\link{mmglm}} and \code{\link{mmpp}}.
}}

\keyword{documentation}


