#    Gaussian with identity link function, mu=beta0
#    one single series
#    Compare dthmm and mmglm1

library(HiddenMarkov)

#------------------------------------------------------------------
#   Using dthmm

#   n = series length for each subject
#   N = number of subjects
n <- 5000
N <- 1

Pi <- matrix(c(0.8, 0.2,
               0.3, 0.7),
             byrow=TRUE, nrow=2)

delta <- c(1, 0)

y <- dthmm(NULL, Pi=Pi, distn="norm", delta=delta, pm=list(mean=c(5, 2), sd=c(1, 1)))

y <- simulate(y, nsim=N*n)
print(logLik(y))

tmp <- BaumWelch(y, bwcontrol(posdiff=FALSE, tol=1e-05))

print(summary(tmp))
print(logLik(tmp))


#------------------------------------------------------------------
#   Using mmglm

glmformula <- formula(y$x ~ 1)
glmfamily <- gaussian(link="identity")
Xdesign <- model.matrix(glmformula)

beta <- matrix(c(5, 2), 
               ncol=ncol(Pi), nrow=ncol(Xdesign), byrow=TRUE)

y2 <- mmglm1(y$x, Pi, delta, glmfamily, beta, Xdesign, sigma=c(1, 1))
print(logLik(y2))

tmp2 <- BaumWelch(y2, bwcontrol(posdiff=FALSE, tol=1e-05))

print(summary(tmp2))
print(logLik(tmp2, fortran=TRUE))
print(logLik(tmp2, fortran=FALSE))


