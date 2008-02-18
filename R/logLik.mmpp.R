"logLik.mmpp" <-
function(object, fortran=TRUE, ...){
    tau <- object$tau[-1] - object$tau[-length(object$tau)]
    m <- nrow(object$Q)
    n <- length(tau)
    Lambda <- diag(object$lambda)
    ##    eigenvalue decomposition
    decomp <- eigen(object$Q-Lambda, symmetric=FALSE)
    if (any(duplicated(decomp$values))) stop("repeated eigenvalues")
    S <- decomp$vectors
    Sinv <- solve(S)
    eigenval <- decomp$values
    ##  scaled forward probabilities
    phi <- as.double(object$delta)
    logalpha <- matrix(as.double(rep(0, m*(n+1))), nrow=(n+1))
    logalpha[1,] <- log(phi)
    scalefac <- as.double(rep(0, n))
    post <- Sinv %*% Lambda
    psi <- array(as.double(0), dim=c(n,m,m))
    if (fortran!=TRUE){
        #   loop3 using R code
        for (i in 1:n){
            psi[i,,] <- S %*% diag(exp(eigenval*tau[i])) %*% post
            phi <- phi %*% psi[i,,]
            sumphi <- sum(phi)
            scalefac[i] <- log(sumphi)
            phi <- phi/sumphi
        }
    } else{
        memory0 <- matrix(as.double(0), ncol=m, nrow=m)
        memory1 <- matrix(as.double(0), ncol=m, nrow=m)
        memory2 <- rep(as.double(0), m)
        if (!is.double(S)) stop("Eigenvectors are not double precision")
        loop3 <- .Fortran("loop3", m, n, phi, S, eigenval, logalpha,
                          scalefac, tau, post, psi, memory0, memory1,
                          memory2,
                          PACKAGE="HiddenMarkov", NAOK=TRUE)
        scalefac <- loop3[[7]]
    }
    return(sum(scalefac))
}

