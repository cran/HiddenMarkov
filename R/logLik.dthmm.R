"logLik.dthmm" <-
function(object, ...){
    #   Log-likelihood of HMM
    #   stationary or non-stationary Markov chain
    #   Generalised from Zucchini Sydney notes
    n <- length(object$x)
    phi <- object$delta
    LL <- 0
    dfunc <- makedensity(object$distn)
    for (i in 1:n){
        if (i > 1) phi <- phi %*% object$Pi
        phi <- phi %*% diag(dfunc(object$x[i], object$pm,
                               getj(object$pn, i)))
        sumphi <- sum(phi)
        LL <- LL + log(sumphi)
        phi <- phi/sumphi
    }
    return(LL)
}

