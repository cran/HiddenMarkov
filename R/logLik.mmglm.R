"logLik.mmglm" <- function(object, ...){
    object <- as.dthmm(object)
    return(logLik.dthmm(object))
}

