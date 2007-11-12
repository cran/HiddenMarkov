#-----  Log Normal Distribution -----
#         test fixed sdlog

library(HiddenMarkov)

n <- 1000
m <- 2
pm=list(meanlog=c(3, 4))
pn=list(sdlog=rep(0.5, n))

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

set.seed(5)
x <- sim.hmm(n, 1, Pi, "lnorm", pm=pm, pn=pn)

y <- Baum.Welch(x$x, Pi, compdelta(Pi), "lnorm",
                pm=pm, pn=pn, maxiter=40)

#   check parameter estimates
print(y$delta)
print(y$pm)
print(y$Pi)
print(sum(y$delta))
print(y$Pi %*% rep(1, ncol(y$Pi)))


#-----  Log Normal Distribution -----
#         test fixed meanlog

n <- 1000
m <- 2
pn=list(meanlog=rep(3, n))
pm=list(sdlog=c(0.8, 0.2))

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

set.seed(5)
x <- sim.hmm(n, 1, Pi, "lnorm", pm=pm, pn=pn)

y <- Baum.Welch(x$x, Pi, compdelta(Pi), "lnorm",
                pm=pm, pn=pn, maxiter=40)

#   check parameter estimates
print(y$delta)
print(y$pm)
print(y$Pi)
print(sum(y$delta))
print(y$Pi %*% rep(1, ncol(y$Pi)))

