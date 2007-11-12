"forwardback" <-
function(x, Pi, delta, distn, pm, pn = NULL){
    m <- nrow(Pi)
    n <- length(x)
    dfunc <- makedensity(distn)
    prob <- NULL
    for (k in 1:m) {
        prob <- cbind(prob,
                      dfunc(x=x, getj(pm, k), pn, log=FALSE))
    }
    #   forward probabilities alpha_ij
    phi <- matrix(delta, nrow=1)
    logalpha <- matrix(rep(NA, m*n), nrow=n)
    lscale <- 0
    for (i in 1:n){
        if (i > 1) phi <- phi %*% Pi
        phi <- phi %*% diag(prob[i,])
        sumphi <- sum(phi)
        phi <- phi/sumphi
        lscale <- lscale + log(sumphi)
        logalpha[i,] <- log(phi) + lscale
    }
    #   log-likelihood
    LL <- lscale
    #   backward probabilities beta_ij
    logbeta <- matrix(rep(NA, m*n), nrow=n)
    logbeta[n,] <- 0
    phi <- matrix(rep(1/m, m), ncol=1)
    lscale <- log(m)
    for (i in seq(n-1, 1, -1)){
        phi <- Pi %*% diag(prob[i+1,]) %*% phi
        logbeta[i,] <- log(phi) + lscale
        sumphi <- sum(phi)
        phi <- phi/sumphi
        lscale <- lscale + log(sumphi)
    }
    return(list(logalpha=logalpha, logbeta=logbeta, LL=LL))
}

