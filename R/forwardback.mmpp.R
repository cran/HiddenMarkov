"forwardback.mmpp" <-
function(tau, Q, delta, lambda){
    m <- nrow(Q)
    n <- length(tau)
    Lambda <- diag(lambda)
    ##    eigenvalue decomposition
    decomp <- eigen(Q-Lambda, symmetric=FALSE)
    if (any(duplicated(decomp$values))) stop("repeated eigenvalues")
    S <- decomp$vectors
    Sinv <- solve(S)
    eigenval <- decomp$values
    ##  scaled forward probabilities
    phi <- matrix(delta, nrow=1)
    logalpha <- matrix(rep(NA, m*(n+1)), nrow=(n+1))
    logalpha[1,] <- log(phi)
    scalefac <- rep(NA, n)
    post <- Sinv %*% Lambda
    psi <- array(NA, dim=c(n,m,m))
    for (i in 1:n){
        psi[i,,] <- S %*% diag(exp(eigenval*tau[i])) %*% post
        phi <- phi %*% psi[i,,]
        sumphi <- sum(phi)
        scalefac[i] <- log(sumphi)
        phi <- phi/sumphi
        logalpha[i+1,] <- log(phi)
    }
    ##  scaled backward probabilities
    logbeta <- matrix(rep(NA, m*(n+1)), nrow=(n+1))
    logbeta[(n+1),] <- 0
    phi <- matrix(rep(1/m, m), ncol=1)
    lscale <- log(m)
    logck <- 0
    for (i in seq(n, 1, -1)){
        phi <- psi[i,,] %*% phi
        logck <- logck + scalefac[i] 
        logbeta[i,] <- log(phi) + lscale - logck
        sumphi <- sum(phi)
        phi <- phi/sumphi
        lscale <- lscale + log(sumphi)
    }
    return(list(logalpha=logalpha, logbeta=logbeta, eigenval=eigenval, S=S,
                Sinv=Sinv, scalefac=scalefac, LL=sum(scalefac)))
}

