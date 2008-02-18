"logLik.dthmm" <-
function(object, fortran=TRUE, ...){
    m <- nrow(object$Pi)
    n <- length(object$x)
    dfunc <- makedensity(object$distn)
    prob <- matrix(as.double(0), nrow=n, ncol=m)
    for (k in 1:m)
        prob[,k] <- dfunc(x=object$x, getj(object$pm, k), object$pn, log=FALSE)
    #   forward probabilities alpha_ij
    phi <- as.double(object$delta)
    logalpha <- matrix(as.double(rep(0, m*n)), nrow=n)
    lscale <- as.double(0)
    if (fortran!=TRUE){
        #  loop1 using R code
        for (i in 1:n){
            if (i > 1) phi <- phi %*% object$Pi
            phi <- phi*prob[i,]
            sumphi <- sum(phi)
            phi <- phi/sumphi
            lscale <- lscale + log(sumphi)
        }
        LL <- lscale
    } else{
        if (!is.double(object$Pi)) stop("Pi is not double precision")
        if (!is.double(prob)) stop("prob is not double precision")
        memory0 <- rep(as.double(0), m)
        loop1 <- .Fortran("loop1", m, n, phi, prob, object$Pi, logalpha,
                          lscale, memory0, PACKAGE="HiddenMarkov")
        LL <- loop1[[7]]
    }
    return(LL)
}

