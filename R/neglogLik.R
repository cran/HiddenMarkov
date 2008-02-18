neglogLik <- function(p, object, updatep){
    object <- updatep(object, p)
    return(-logLik(object))
}
