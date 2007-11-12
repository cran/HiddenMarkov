"mmpp" <-
function (tau, Q, delta, lambda, nonstat = TRUE)
{
    y <- c(list(tau=tau, Q=Q, delta=delta, lambda=lambda,
                nonstat=TRUE))
    class(y) <- "mmpp"
    return(y)
}

