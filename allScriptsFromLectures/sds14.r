######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 14: Law of large numbers, and the central limit theorem

par(mar=c(4,4,1,1))

# Markov's inequality
lambda = 2 # X ~ Exp(lambda)
mu = 1/lambda # E[X] = 1/lambda
curve(dexp(x, lambda), xlim=c(0, 5))
a = seq(0.1, 5, 0.1)
pr = 1 - pexp(a, lambda) # P(X>a)
plot(a, pr, ylim=c(0, 0.6))
# large upper bound!
lines(a, mu/a, col=2)  # see help(lines)

# Chebyshev's inequality
var = 1/lambda^2 # Var(X) = 1/lambda^2
lines(a, var/a^2, col=3) # better upper bound for a > 1

mu = 0; sigma = 1
# normal distribution
curve(dnorm(x, mu, sigma), xlim=c(mu-5,mu+5))
a = seq(0.1, 5, 0.1)
pr = pnorm(mu-a, mu, sigma) + (1-pnorm(mu+a, mu, sigma)) # P(|Y-mu|>a)
pr = 2*pnorm(mu-a, mu, sigma) # equivalent because normal distribution is symmetric
plot(a, pr)
lines(a, 1/a^2 * sigma^2, col=2) # large upper bound! Especially for a < 1
# P(|Y-mu|<=k*sigma) >= 1 - 1/k^2
k = seq(0.1, 5, 0.1)
pr = pnorm(mu+k*sigma, mu, sigma) - pnorm(mu-k*sigma, mu, sigma) # P(|Y-mu|< k*sigma)
plot(k, pr)
lines(k, 1-1/k^2, col=2) # small lower bound! Even negative for k < 1

# Law of large numbers

# Exercise - Fig. 13.2 from [B1]: plot mean(rnomr(n)) for n in 1:1000
x = seq(1, 1000, 10)
# N(5, 4)
y = sapply(x, function(n) mean(rnorm(n, 5, 4)))
plot(x, y, ylim=c(3,7)) 
abline(h=5, col=2, lwd=2) # straight line, see help(abline)

# Bin(10, 0.5) E[X] = 10*0.5 = 5
y = sapply(x, function(n) mean(rbinom(n, 10, 0.5)))
plot(x, y, ylim=c(4,6)) 
abline(h=5, col=2, lwd=2)

# Cauchy(0, 1) E[X] does not exist!
y = sapply(x, function(n) mean(rcauchy(n)))
plot(x, y) 

# Estimating probabilities

# joint probability P(C=c,A=a) = P(Y=c|A=a)P(A=a)
# with P(A=a) ~ Bin(5, p) and P(C=c|A=a) ~ Ber(0.50+a*0.01)
n = 10000; p=0.5
ra = rbinom(n, 5, p)
dbinom(0:5, 5, p)
rc = sapply(ra, function(a) rbinom(1, 1, 0.5+a*0.01))
df = data.frame(ra, rc); colnames(df)=c("A", "C")
View(df)

# estimating p=P(C=1|A=2)
a = 2; c = 1
# theoretical Ber(0.50+a*0.01) means p = 0.52
truep = dbinom(c, 1, 0.5+a*0.01)
truep
# estimate
pac = mean(df$A==a & df$C==c)
pa = mean(df$A==a)
pac/pa

# target encoding of A=a
encoding = tapply(df$C, df$A, mean)
encoding
df$encodedA = encoding[df$A+1]
View(df)

# Central Limit Theorem

# from a Gaussian
n = 50
xns = replicate(1000, mean(rnorm(n, 1, 2)) )
hist(xns, breaks=30, freq=FALSE, ylim=c(0,3))
curve(dnorm(x, 1, 2/sqrt(n)), add=TRUE, col=2, lwd=2)
n = 100
xns = replicate(1000, mean(rnorm(n, 1, 2)) )
hist(xns, breaks=30, freq=FALSE, add=T, col=3)
curve(dnorm(x, 1, 2/sqrt(n)), add=TRUE, col=4, lwd=2)

# from an Exponential
# Exp(2) E[X] = 0.5 Var[X] = 0.5^2
xns = replicate(1000, mean(rexp(n, 2)) )
hist(xns, breaks=30, freq=FALSE)
curve(dnorm(x, 0.5, 0.5/sqrt(n)), add=TRUE, col="red", lwd=2)

# mean, sigma and n are parameters
central.limit = function(xns, mu=0, sigma=1, n=1, ...)
{
  h = hist(xns, plot=FALSE, breaks="Scott", ...)
  ylim = range(0, h$density, dnorm(mu, mu, sigma/sqrt(n)))
  hist(xns, freq=FALSE, breaks="Scott", ylim=ylim, xlab=deparse(substitute(xns)), ...) 
  curve(dnorm(x, mu, sigma/sqrt(n)), add=TRUE, col="red", lwd=2)
}

# central limit theorem - Exp(2)
xns = replicate(1000, mean(rexp(n, 2)) )
central.limit(xns, 0.5, 0.5, n)

# central limit theorem - Unif(-1, 1)
xns = replicate(1000, mean(runif(n, -1, 1)) )
central.limit(xns, 0, 2/sqrt(12), n)

# central limit theorem - Binom(100, 0.5)
xns = replicate(1000, mean(rbinom(n, 100, 0.5)) )
central.limit(xns, 100*0.5, sqrt(100*0.25), n)

# z transformation
z.transf = function(xn, mu, sigma, n) (sqrt(n)*(xn-mu)/sigma)

# now use standard normal - Unif(-1, 1)
xns = replicate( 1000, mean(runif(n, -1, 1)) )
zns = z.transf(xns, 0, 2/sqrt(12), n)
central.limit(zns)

# central limit theorem- Binom(100, 0.5)
xns = replicate(1000, mean(rbinom(n, 100, 0.5)) )
zns = z.transf(xns, 100*0.5, sqrt(100*0.25), n)
central.limit(zns)

# approximating probabilities

# let X_i ~ Exp(lambda=2)
n = 100; lambda = 2
mu = 1/lambda # expectation 
sigma = 1/lambda # stdev
# What is P((X1+...+Xn)/n >= 0.6)?
true_answer = 1-pgamma(0.6*n, n, lambda)
true_answer
# approx using CLT
z = (0.6-mu)/(sigma/sqrt(n))
# or
z = z.transf(0.6, mu, sigma, n)
approx_answer = 1-pnorm(z)
approx_answer
# true of (X1+...+Xn)/n using change-of-units
curve(n*dgamma(n*x, n, lambda), xlim=c(0, 1)) 
# approximation (X1+...+Xn)/n ~ N(mu, sigma^2/n)
curve(dnorm(x, mu, sigma/sqrt(n)), add=T, col=2)
# true df of X1+...+Xn since X_i ~ Exp(lambda=2)
curve(dgamma(x, n, lambda), xlim=c(0, n)) 
# approximation (X1+...+Xn) ~ N(mu*n, n*sigma^2)
curve(dnorm(x, mu*n, sqrt(n)*sigma), xlim=c(0, n), add=T, col=2)
