"Viterbihmm" <-
function (x, Pi, delta, distn, pm, pn = NULL) 
{
    .Deprecated("Viterbi", package="HiddenMarkov",
          msg="'Viterbihmm' is deprecated.
          Use the dthmm model object with 'Viterbi', see help('Viterbi').")
    dfunc <- makedensity(distn)
    n <- length(x)
    m <- nrow(Pi)
    nu <- matrix(NA, nrow = n, ncol = m)
    y <- rep(NA, n)
    nu[1, ] <- log(delta) + dfunc(x=x[1], pm, getj(pn, 1),
                                  log=TRUE)
    logPi <- log(Pi)
    for (i in 2:n) {
        matrixnu <- matrix(nu[i - 1, ], nrow = m, ncol = m)
        nu[i, ] <- apply(matrixnu + logPi, 2, max) +
                      dfunc(x=x[i], pm, getj(pn, i),
                                  log=TRUE)
    }
    if (any(nu[n, ] == -Inf)) 
        stop("Problems With Underflow")
    y[n] <- which.max(nu[n, ])
    for (i in seq(n - 1, 1, -1)) y[i] <- which.max(logPi[, y[i + 
        1]] + nu[i, ])
    return(y)
}
