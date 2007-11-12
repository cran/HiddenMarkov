"residuals.dthmm" <-
function (object, ...) 
{
    prob <- do.call("probhmm", object)
    if (object$discrete != FALSE){
        prob1 <- do.call("probhmm", c(object, adj=1))
        prob <- (prob + prob1)/2
    }
    return(qnorm(prob))
}

