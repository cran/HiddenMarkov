"residuals.mmglm" <-
function (object, ...) 
{
    object <- as.dthmm(object)
    return(residuals.dthmm(object))
}

