"sim.hmm" <-
function (n, initial, Pi, distn, pm, pn = NULL) 
{
    .Deprecated("simulate", package="HiddenMarkov",
          msg="'sim.hmm' is deprecated.
          Use 'simulate' instead, see help('simulate').")
    rname <- paste("r", distn, sep="")
    y <- sim.markov(n, initial, Pi)
    x <- rep(NA, n)
    for (i in 1:n) {
        x[i] <- do.call(rname, c(list(n=1), getj(pm, y[i]),
                        getj(pn, i)))
    }
    return(list(x = x, y = y))
}

