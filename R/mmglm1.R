"mmglm1" <-
function (y, Pi, delta, glmfamily, beta, Xdesign,
          sigma=NA, nonstat=TRUE, size=NA, msg=TRUE){
    if (msg)
        message("NOTE: 'mmglm1' and its methods are still under development and may change.")
    if (glmfamily$family=="binomial" & all(is.na(size)))
        stop("Argument size must be specified when fitting a binomial model.")
    x <- c(list(y=y, Pi=Pi, delta=delta, glmfamily=glmfamily,
                beta=beta, Xdesign=Xdesign, sigma=sigma,
                size=size, nonstat=nonstat))
    class(x) <- c("mmglm1")
    return(x)
}


