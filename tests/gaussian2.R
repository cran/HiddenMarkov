#   Checks against Peter Thomson's program

library(HiddenMarkov)

n <- 1000
m <- 2
p <- c(0, 2)

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

set.seed(5)
x <- sim.hmm(n, 1, Pi, "norm", pm=list(mean=p),
             pn=list(sd=rep(1, n)))

plot(1:n, x$x, type="l", xlab="Time", ylab="X",
     main=paste("Gaussian", m, "State HMM Fixed Variance"))
for (j in 1:m) points((1:n)[x$y==j], x$x[x$y==j], col=j+1)

cat(paste(x$y, x$x), sep="\n", file="gaussian2.txt")

y <- Baum.Welch(x$x, Pi, compdelta(Pi), "norm",
                pm=list(mean=p, sd=c(1, 1)),
                nonstat=FALSE, maxiter=39)

#   check parameter estimates
print(y$delta)
print(y$pm)
print(y$Pi)
print(sum(y$delta))
print(y$Pi %*% rep(1, ncol(y$Pi)))

