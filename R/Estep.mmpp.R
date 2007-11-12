"Estep.mmpp" <-
function (tau, Q, delta, lambda) 
{
    #   scaled version of Ryden (1996)
    m <- ncol(Q)
    n <- length(tau)
    x <- forwardback.mmpp(tau, Q, delta, lambda)
    logbeta <- x$logbeta
    logalpha <- x$logalpha
    alpha <- exp(logalpha)
    beta <- exp(logbeta)
    #   eigenvalue decomposition
    d <- x$eigenval
    expd <- exp(d)
    S <- x$S
    Sinv <- x$Sinv
    diff <- outer(d, d, FUN="-")+diag(m)
    post0 <- Sinv %*% diag(lambda)
    A <- matrix(0, nrow=m, ncol=m)
    TT <- array(NA, dim=c(n, m, m))
    for (k in 1:n){
        expdtau <- exp(d*tau[k])
#  this causes about 10^-12 difference in diff
#        expdtau <- expd^tau[k]
        difftau <- outer(expdtau, expdtau, FUN="-")
        TT[k,,] <- (difftau + diag(tau[k]*expdtau))/
                    diff/exp(x$scalefac[k])
    }
    for (i in 1:m){
        pre <- S %*% diag(Sinv[,i])
        for (j in 1:m){
            post <- diag(S[j,]) %*% post0
            for (k in 1:n){
                A[i,j] <- A[i,j] + alpha[k,] %*% pre %*% TT[k,,] %*%
                                   post %*% beta[(k+1),]
            }
        }
    }
    if (n==1) B <- exp(logalpha[-1,]+logbeta[-1,])
    else B <- apply(exp(logalpha[-1,]+logbeta[-1,]), MARGIN=2, FUN=sum)
    return(list(A=A, B=B, logalpha=logalpha, logbeta=logbeta, LL=x$LL))
}

