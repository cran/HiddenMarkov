"sim.markov" <-
function(n, initial, Pi){
    .Deprecated("simulate", package="HiddenMarkov",
          msg="'sim.markov' is deprecated.
          Use 'simulate' instead, see help('simulate').")
    #    simulate a Markov Chain
    x <- rep(NA, n)
    x[1] <- initial
    m <- nrow(Pi)
    for (i in 2:n)
        x[i] <- sample(x=1:m, size=1, prob=Pi[(x[i-1]),])
    return(x)
}

