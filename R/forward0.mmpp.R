"forward0.mmpp" <-
function(tau, Q, delta, lambda){
    #  forward probs for MMPP
    #  tau contains the interevent times
    .Deprecated("forwardback.mmpp", package="HiddenMarkov",
          msg="'forward0.mmpp' is deprecated.
          Use 'forwardback.mmpp' instead, see help('forwardback.mmpp').")
    m <- nrow(Q)
    n <- length(tau)
    Lambda <- diag(lambda)
    phi <- matrix(delta, nrow=1)
    logalpha <- matrix(rep(NA, m*(n+1)), nrow=(n+1))
    logalpha[1,] <- log(phi)
    lscale <- 0
    decomp <- eigen(Q-Lambda, symmetric=FALSE)
    if (any(duplicated(decomp$values))) stop("repeated eigenvalues")
    S <- decomp$vectors
    Sinv <- solve(S)
    eigenval <- decomp$values
    for (i in 2:(n+1)){
        phi <- phi %*% S %*% diag(exp(eigenval*tau[i-1])) %*%
                 Sinv %*% Lambda
#       phi <- phi %*% matrixexp((Q-Lambda)*tau[i-1]) %*% Lambda
        sumphi <- sum(phi)
        phi <- phi/sumphi
        lscale <- lscale + log(sumphi)
        logalpha[i,] <- log(phi) + lscale
    }
    return(Re(logalpha))
}

