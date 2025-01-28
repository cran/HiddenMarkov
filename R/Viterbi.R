Viterbi <- function (object, ...){
    if (inherits(object, "numeric"))
        do.call(Viterbihmm, list(object, ...))
    else if (inherits(object, "mmpp"))
        stop("Viterbi does not yet have a method for objects of class 'mmpp'.")
    else UseMethod("Viterbi")
}

