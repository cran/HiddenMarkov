"residualshmm" <-
function (x, Pi, delta, distn, pm, pn = NULL, discrete = FALSE) 
{
    .Deprecated("residuals", package="HiddenMarkov",
          msg="'residualshmm' is deprecated.
          Use 'residuals' instead, see help('residuals').")
    prob <- probhmm(x, Pi, delta, distn, pm, pn)
    if (discrete != FALSE){
        prob1 <- probhmm(x, Pi, delta, distn, pm, pn, adj=1)
        prob <- (prob + prob1)/2
    }
    return(qnorm(prob))
}
