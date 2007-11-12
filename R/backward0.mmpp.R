"backward0.mmpp" <-
function(tau, Q, lambda){
    #   backward probabilities for MMPP
    #  tau contains the interevent times
    .Deprecated("forwardback.mmpp", package="HiddenMarkov",
          msg="'backward0.mmpp' is deprecated.
          Use 'forwardback.mmpp' instead, see help('forwardback.mmpp').")
    m <- nrow(Q)
    n <- length(tau)
    Lambda <- diag(lambda)
    logbeta <- matrix(rep(NA, m*(n+1)), nrow=(n+1))
    logbeta[(n+1),] <- 0
    phi <- matrix(rep(1/m, m), ncol=1)
    lscale <- log(m)
    decomp <- eigen(Q-Lambda, symmetric=FALSE)
    if (any(duplicated(decomp$values))) stop("repeated eigenvalues")
    S <- decomp$vectors
    Sinv <- solve(S)
    eigenval <- decomp$values
    for (i in seq(n, 1, -1)){
        phi <- S %*% diag(exp(eigenval*tau[i])) %*%
                 Sinv  %*% Lambda %*% phi
#       phi <- matrixexp((Q-Lambda)*tau[i]) %*% Lambda %*% phi
        logbeta[i,] <- log(phi) + lscale
        sumphi <- sum(phi)
        phi <- phi/sumphi
        lscale <- lscale + log(sumphi)
    }
    return(Re(logbeta))
}

