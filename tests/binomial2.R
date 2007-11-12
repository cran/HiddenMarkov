#   Checks against Junko Murakami's program

library(HiddenMarkov)

n <- 1000
m <- 2
p <- c(0.2, 0.6)

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

set.seed(5)
x <- sim.hmm(n, 1, Pi, "binom", pm=list(prob=p),
             pn=list(size=rep(1, n)))

cat(paste(x$y, x$x), sep="\n", file="binomial2.txt")

if (0==1){
y <- Baum.Welch(x$x, Pi, c(1, 0), "binom",
                pm=list(prob=p),
                pn=list(size=rep(1, n)),
                tol=1e-9, maxiter=2711)

#   check parameter estimates
print(y$delta)
print(y$pm)
print(y$Pi)
print(sum(y$delta))
print(y$Pi %*% rep(1, ncol(y$Pi)))
}


