"Baum.Welch" <-
function (x, Pi, delta, distn, pm, pn = NULL, nonstat = TRUE,
          maxiter = 500, tol = 1e-05, prt = TRUE,
          posdiff = (distn[1]!="glm")) 
{
    .Deprecated("BaumWelch", package="HiddenMarkov",
          msg="'Baum.Welch' is deprecated.
          Use 'BaumWelch' instead, see help('BaumWelch').")
    if (distn[1]!="glm"){
        Mstep <- parse(text=paste("Mstep.", distn,
                       "(x, cond, pm, pn)", sep=""))
    } else{
        Mstep <- parse(text=paste("Mstep.glm",
                  "(x, cond, pm, pn, distn[2], distn[3])", sep=""))
    }
    m <- nrow(Pi)
    n <- length(x)
    oldLL <- -Inf
    for (iter in 1:maxiter) {
        cond <- Estep(x, Pi, delta, distn, pm, pn)
        diff <- cond$LL - oldLL
        if (prt) {
            cat("iter =", iter, "\n")
            cat("LL =", formatC(cond$LL, digits=log10(1/tol)+2,
                                format="f"), "\n")
            cat("diff =", diff, "\n\n")
        }
        if (diff < 0 & posdiff) stop("Worse log-likelihood")
        if (abs(diff) < tol) break
        #----  Mstep  ----
        Pi <- diag(1/apply(cond$v, MARGIN = 2, FUN = sum)) %*% 
            apply(cond$v, MARGIN = c(2, 3), FUN = sum)
        if (nonstat) delta <- cond$u[1, ]
        else delta <- compdelta(Pi)
        pm <- eval(Mstep)
        oldLL <- cond$LL
    }
    return(list(delta = delta, Pi = Pi, u = cond$u, 
           v = cond$v, pm=pm, LL = cond$LL,
           iter = iter, diff = diff))
}
