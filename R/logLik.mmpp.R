"logLik.mmpp" <- function(object, ...){
    #   Log-likelihood of MMPP
    n <- length(object$tau)-1
    tau <- object$tau[-1] - object$tau[-(n+1)]
    Lambda <- diag(object$lambda)
    phi <- object$delta
    LL <- 0
    decomp <- eigen(object$Q-Lambda, symmetric=FALSE)
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

