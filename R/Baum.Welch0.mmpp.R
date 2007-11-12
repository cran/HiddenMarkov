"Baum.Welch0.mmpp" <-
function (tau, Q, delta, lambda, nonstat = TRUE,
          maxiter = 500, tol = 1e-05, prt = TRUE,
          converge=expression(diff < tol)) 
{
    #  Using the method in Ryden (1996) - unscaled
    .Deprecated("BaumWelch", package="HiddenMarkov",
          msg="'Baum.Welch0.mmpp' is deprecated.
          Use 'BaumWelch' instead, see help('BaumWelch').")
    m <- nrow(Q)
    n <- length(tau)
    oldLL <- -Inf
    for (iter in 1:maxiter) {
        cond <- Estep0.mmpp(tau, Q, delta, lambda)
#       overflow problems
#       LL <- log(sum(exp(cond$logalpha[(n+1),])))
        LL <- logLikmmpp(tau, Q, delta, lambda)
        diff <- LL - oldLL
        if (prt) {
            cat("iter =", iter, "\n")
            cat("LL =", formatC(LL, digits=log10(1/tol)+2,
                                format="f"), "\n")
            cat("diff =", diff, "\n\n")
        }
        if (diff < 0){
            stop("Worse log-likelihood on last iteration")
        }
        if (eval(converge)) break
        #----  Mstep  ----
        Q <- Q * (diag(1/diag(cond$A)) %*% cond$A)
        diag(Q) <- 0
        diag(Q) <- -apply(Q, MARGIN=1, FUN=sum)
        lambda <- cond$B/diag(cond$A)
        if (nonstat) delta <- exp(cond$logalpha[1, ] +
                                  cond$logbeta[1, ] - LL)
        else delta <- compdelta(solve(diag(lambda) -
                                Q) %*% diag(lambda))
        oldLL <- LL
    }
    return(list(delta = delta, Q = Q, lambda = lambda, 
           LL = LL, iter = iter, diff = diff))
}



