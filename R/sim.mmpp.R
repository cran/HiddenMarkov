"sim.mmpp" <-
function (n, initial, Q, lambda) 
{
    #   ********   VALID FOR >=2 STATES   ********
    .Deprecated("BaumWelch", package="HiddenMarkov",
          msg="'sim.mmpp' is deprecated.
          Use 'simulate' instead, see help('simulate').")
    #   y    contains sequence of Markov states
    #   x    transition time to next state
    #   tau  times of Poisson events
    m <- ncol(Q)
    Pi <- diag(m) - diag(1/diag(Q)) %*% Q
    ys <- rep(NA, n+1)
    tau <- rep(NA, n+1)
    #    the length of x and y may be too short
    #    gets extended later if required
    x <- rep(NA, n*10)
    y <- rep(NA, n*10)
    y[1] <- ys[1] <- initial
    x[1] <- tau[1] <- 0
    i <- j <- 2
    while (TRUE){
        #   sim time spent in Markov state y[i-1]
        y[i] <- sample(x=1:m, size=1, prob=Pi[(y[i-1]),])
        x[i] <- x[i-1] + rexp(1, rate=-Q[y[i-1], y[i-1]])
        t0 <- x[i-1]
        while(TRUE){
            #   sim times of Poisson events
            ti <- t0 + rexp(1, rate=lambda[y[i-1]])
            if (ti < x[i]){
                tau[j] <- t0 <- ti
                ys[j] <- y[i-1]
                j <- j + 1
                if (j==n+2)
                    return(list(x=x[1:i], y=y[1:i], tau=tau, ys=ys))
            }
            else break
        }
        i <- i+1
        #    extend x and y if too short
        if (i > length(x)){
            x <- c(x, rep(NA, n*10))
            y <- c(y, rep(NA, n*10))
        }
    }
}

