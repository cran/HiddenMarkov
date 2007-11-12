"sim.hmm1" <-
function (n, initial, Pi, distn, pm) 
{
    .Deprecated("simulate", package="HiddenMarkov",
          msg="'sim.hmm1' is deprecated.
          Use 'simulate' instead, see help('simulate').")
    rfunc <- eval(parse(text=paste("r", distn, sep="")))
    nms <- names(pm)
    newargs <- paste(nms, "=", "pm[[", seq(1,length(nms)),
                     "]][k]", sep="", collapse=",")
    newargs <- paste("alist(n=, ", newargs, ")", sep="")
    formals(rfunc) <- eval(parse(text=newargs))
    y <- sim.markov(n, initial, Pi)
    x <- rep(NA, n)
    m <- nrow(Pi)
    for (k in 1:m) {
        a <- (y == k)
        x[a] <- rfunc(n=sum(a))
    }
    return(list(x = x, y = y))
}

