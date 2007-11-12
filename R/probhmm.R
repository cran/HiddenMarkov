"probhmm" <-
function (x, Pi, delta, distn, pm, pn = NULL, adj = 0, ...) 
{
    pfunc <- makedistn(distn)
    n <- length(x)
    logbeta <- backward(x, Pi, distn, pm, pn)
    logalpha <- forward(x, Pi, delta, distn, pm, pn)
    prob <- rep(NA, n)
    for (i in 1:n) {
        if (i==1){
            pre <- delta
        }
        else {
            la <- logalpha[i-1,]
            pre <- exp(la - mean(la[la != -Inf])) %*% Pi
        }
        lb <- logbeta[i,]
        post <- exp(lb - mean(lb[lb != -Inf]))
        prob[i] <- (pre %*% diag(pfunc(x[i]-adj, pm,
                     getj(pn, i))) %*% post)/(pre %*% post)
    }
    return(prob)
}
