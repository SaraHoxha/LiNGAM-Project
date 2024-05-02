######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 12: Simulation ##########

runif(5) # 5 random numbers ~ U(0,1)
runif(5)
set.seed(0) # set seed in random number generation
runif(5)
set.seed(0)
runif(5)

# from U(0, 1) to Ber(p), equivalent to rbinom(nr, 1, p)
u2ber = function(nr, p) {
  ru = runif(nr)
  rb = as.integer(ru < p)
  return(rb)
}
rb = u2ber(10000, 0.75)
pmf = table(rb)/length(rb) 
pmf

# from Ber to Binom, equivalent to rbinom(nr, n, p)
ber2bin = function(nr, n, p) {
  rbi = rep(0, nr) # initialize to zero
  for(i in 1:n) {
    rbi = rbi + u2ber(nr, p)
  }
  return(rbi)
}
n = 10
p = 0.5
rbi = ber2bin(10000, n, p)
pmf = table(rbi)/length(rbi) 
plot(pmf, type='p', pch=16)
points(0:n, dbinom(0:n, n, p), col=2, pch=16)

# from Unif to Categorical
u2cat = function(nr, v) {
  v1 = c(-0.1, cumsum(v))
  ru = runif(nr)
  rb = as.integer(cut(ru, breaks=v1, labels=0:(length(v)-1)))-1
  return(rb)
}
rb = u2cat(10000, c(0.2, 0.5, 0.3))
pmf = table(rb)/length(rb) 
plot(pmf, type='p', pch=16)
# equivalent to
sample(0:2, 10000, replace=TRUE, prob=c(0.2, 0.5, 0.3))

# multinomial distribution with n=1
# produces one-hot encoded vectors
rmultinom(5, 1, c(0.2, 0.5, 0.3))
rmultinom(5, 3, c(0.2, 0.5, 0.3))

# Exercise at home: generate Geo(p) from Ber(p)
# Exercise at home: generate Mult(n, p) from Cat(p)
# Exercise at home: generate Nbin(n, p) from Geo(p)
# Exercise at home: generate Poi(mu) from Bin(n, mu/n)

# realizations ~ U(0,1)
ru = runif(10000)

# from Unif to Exp
lambda = 2
re = 1/lambda * log(1/(1-ru)) # explicit transformation 
plot(ecdf(re), lwd=2) # CDF
curve(pexp(x,lambda), col=2, add=T, lwd=2) # theoretical CDF
# equivalent to
plot(ecdf(rexp(10000, lambda)), lwd=2, col=3, add=T)
# pdf
hist(re, breaks=60, freq=FALSE) # histogram of re
curve(dexp(x,lambda), col=2, add=T, lwd=3) # theoretical DF

# from Exp(l) to Erl(n, l)
n = 100; l = 0.5
re = replicate(10000, sum(rexp(n, l)))
hist(re, breaks=60, freq=FALSE) # histogram of re
curve(dgamma(x, n, l), col=2, add=T, lwd=3) # theoretical DF

# from Norm to Cauchy
curve(dcauchy(x), xlim=c(-3, 3))
r1 = rnorm(1000)
r2 = rnorm(1000)
rc = r1/r2 # equivalent to rcauchy(1000)
plot(ecdf(rc), xlim=c(-5, 5))
curve(pcauchy(x), col=2, add=T, lwd=3) # theoretical CDF
hist(rc, xlim=c(-5,5), ylim=c(0,0.35), breaks=10000, probability=T)
curve(dcauchy(x), add=T, col=2, lwd=3) # theoretical df

# from Norm to LogNorm Y = exp(X) for X ~ N(mu, sigma^2), i.e., log(Y) ~ N(mu, sigma^2)
r= rnorm(1000)
rc = exp(r) # equivalent to rlnorm(1000)
plot(ecdf(rc), xlim=c(0, 10))
curve(plnorm(x), col=2, add=T, lwd=3) # theoretical CDF
hist(rc, xlim=c(0,10), ylim=c(0,0.7), breaks=100, probability=T)
curve(dlnorm(x), add=T, col=2, lwd=3) # theoretical df

# mixture of two Gaussians
dmix = function(x) .3*dnorm(x) + .4*dnorm(x, 3, .7) + .3*dnorm(x, 8, 2)
curve(dmix(x), xlim=c(-2, 10))

# wrong way: the sum of two independent normals is still a normal!
n = 10000
rm = .3*rnorm(n) + .4*rnorm(n, 3, .7) + .4*rnorm(n, 8, 2)
hist(rm, xlim=c(-2,10), ylim=c(0,0.45), breaks=100, probability=T)

# correct way
components = sample(1:3,prob=c(.3, .4, .4),size=n,replace=TRUE)
mus = c(0, 3, 8)
sds = c(1, .7, 2)
rm = rnorm(n, mus[components], sds[components])
hist(rm, xlim=c(-2,10), ylim=c(0,0.25), breaks=100, probability=T)
curve(dmix(x), add=T, col=2, lwd=2)

# general way (as far as CDF is known and invertible)
pmix = function(x) .3*pnorm(x) + .4*pnorm(x, 3, .7) + .3*pnorm(x, 8, 2)
curve(pmix(x), xlim=c(-2, 10))
# inverse (analytical or numeric)
library(GoFKernel) # for numeric inverse
pmix.inv = Vectorize(inverse(pmix))
rm = pmix.inv(runif(n))
hist(rm, xlim=c(-2,10), ylim=c(0,0.25), breaks=100, probability=T)
curve(dmix(x), add=T, col=2, lwd=2)


# Dataset generation through simulation - preliminary idea
#
# sex ~ Ber(0.5)
# P(sport|sex)
# sport/sex     0   1
#           0  .15 .15
#           1  .25 .0
#           2  .25 .5
#           3  .35 .35
# hg ~ 13.56 + 15.5*sex  - hemoglobin concentration

n = 10000
sex = rbinom(n, 1, 0.5)
P_sport_0 = sample(0:3, n, prob=c(0.15, 0.25, 0.25, 0.35), replace=TRUE) 
P_sport_1 = sample(0:3, n, prob=c(0.15, 0.0, 0.5, 0.35), replace=TRUE) 
sport = (1-sex)*P_sport_0 + sex*P_sport_1
hg = 13.56 + 15.5*sex

df = data.frame(sex, sport, hg)
View(df)
