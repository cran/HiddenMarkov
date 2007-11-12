"logLikmmpp" <-
function(tau, Q, delta, lambda){
    #   Log-likelihood of MMPP
    .Deprecated("logLik", package="HiddenMarkov",
          msg="'logLikmmpp' is deprecated.
          Use 'logLik' instead, see help('logLik').")
    n <- length(tau)
    Lambda <- diag(lambda)
    phi <- delta
    LL <- 0
    decomp <- eigen(Q-Lambda, symmetric=FALSE)
    if (any(duplicated(decomp$values))) stop("repeated eigenvalues")
    S <- decomp$vectors
    post <- solve(S) %*% Lambda
    eigenval <- decomp$values
    for (i in 1:n){
        phi <- phi %*% S %*% diag(exp(eigenval*tau[i])) %*%
                 post
        sumphi <- sum(phi)
        LL <- LL + log(sumphi)
        phi <- phi/sumphi
    }
    return(LL)
}

