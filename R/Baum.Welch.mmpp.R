"Baum.Welch.mmpp" <-
function (tau, Q, delta, lambda, nonstat = TRUE,
          maxiter = 500, tol = 1e-05, prt = TRUE,
          converge=expression(diff < tol)) 
{
    #   scaled version of Ryden (1996)
    .Deprecated("BaumWelch", package="HiddenMarkov",
          msg="'Baum.Welch.mmpp' is deprecated.
          Use 'BaumWelch' instead, see help('BaumWelch').")
    m <- nrow(Q)
    n <- length(tau)
    oldLL <- -Inf
    for (iter in 1:maxiter) {
        cond <- Estep.mmpp(tau, Q, delta, lambda)
        LL <- cond$LL
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
                                  cond$logbeta[1, ])
        else delta <- compdelta(solve(diag(lambda) -
                                Q) %*% diag(lambda))
        oldLL <- LL
    }
    return(list(delta = delta, Q = Q, lambda = lambda,
           LL = LL, iter = iter, diff = diff))
}



