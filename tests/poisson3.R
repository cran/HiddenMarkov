#   Compare with Mark Bebbington's program

library(HiddenMarkov)


n <- 1000
m <- 3
p <- c(0.1, 2, 5)

Pi <- matrix(c(0.8, 0.2, 0.0,
               0.3, 0.4, 0.3,
               0.0, 0.3, 0.7),
             byrow=TRUE, nrow=3)

set.seed(5)
x <- sim.hmm(n, 1, Pi, "pois", pm=list(lambda=p))

cat(paste(x$y, x$x), sep="\n", file="poisson3.txt")

y <- Baum.Welch(x$x, Pi, c(1, 0, 0), "pois",
                pm=list(lambda=p), tol = 1e-8,
                maxiter=46)

#   check parameter estimates
print(y$delta)
print(y$pm)
print(y$Pi)
print(sum(y$delta))
print(y$Pi %*% rep(1, ncol(y$Pi)))

