"mmglm" <-
function (x, Pi, delta, family, link, beta, glmformula=formula(y~x1),
          sigma=NA, nonstat = TRUE)
{
    x <- c(list(x=x, Pi=Pi, delta=delta, family=family, link=link,
                beta=beta, glmformula=glmformula, sigma=sigma,
                nonstat=nonstat))
    class(x) <- c("mmglm")
    return(x)
}

