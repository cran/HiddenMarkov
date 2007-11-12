BaumWelch.mmglm <- function (object, control = bwcontrol(), ...){
    object <- as.dthmm(object)
    object <- BaumWelch.dthmm(object, control)
    return(as.mmglm(object))
}

