"dthmm" <-
function (x, Pi, delta, distn, pm, pn = NULL, discrete = FALSE,
          nonstat = TRUE)
{
    y <- c(list(x=x, Pi=Pi, delta=delta, distn=distn, pm=pm,
                pn=pn, discrete=discrete, nonstat=TRUE))
    class(y) <- "dthmm"
    return(y)
}

