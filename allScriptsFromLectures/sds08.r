######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 08: Continuous Distributions ##########

# from discrete Uniform to Uniform
a = 18; b = 30
1/(b-a) # constant
curve(dunif(x, a, b), xlim=c(a-1, b+1), col=2, lwd=2) # uniform pdf
curve(punif(x, a, b), xlim=c(a-1, b+1)) # uniform cdf

# from Geom to Exp
p = 0.2 # unfair coin
k = 1:20
plot(k, pgeom(k-1, p), xlim=c(0, max(k)), ylim=c(0, 1)) # ATTENTION: k-1 and not k
lambda = -log(1-p)
curve(pexp(x, lambda), add=T, col=2, lwd=2) # exponential pdf
curve(dexp(x, lambda), xlim=c(0, 20), col=2, lwd=2) # exponential df
curve(dexp(x, lambda), log="y", xlim=c(0, 20), col=2, lwd=2) # exponential df

# from Bin with Norm
p = 0.5 # fair coin
n = 20 # number of trials
k = 0:n # sum of successes
plot(k, dbinom(k, n, p), pch=16, xlim=c(-1, n+1)) # dbinom R function for pmf
mu = n*p; sigma = sqrt(n*p*(1-p)) # normal approximation of the binomial
curve(dnorm(x, mu, sigma), add=T, col=2, lwd=2) 
help(dnorm) # ATTENTION: N(mu, sigma), and not N(mu, sigma^2)
curve(dnorm(x), xlim=c(-2, 2))
segments(1.04, 0, 1.04, dnorm(1.04), col=2)
# P(Z > 1.04)
1-pnorm(1.04) # standard ccdf (as table in appendix)
# standard ccdf using numeric integration
integrate(dnorm, 1.04, Inf)

# quantiles
qexp(.95, lambda) # .95 quantile
curve( dexp(x, lambda), xlim=c(0, 20), col=2, lwd=2) # exponential df
curve( qexp(x, lambda), xlim=c(0,1)) # exponential quantiles
curve( log(1/(1-x))/lambda, add=T, col="red") # closed form for quantiles

p
curve(pgeom(x-1, p), xlim=c(0,n)) # pgeom R function for cdf
curve( qgeom(x, p), xlim=c(0,1)) # quantiles apply to discrete distributions as well
curve( pbinom(x, 1, 0.7), xlim=c(-0.1, 1.1)) # Ber(0.7)
qbinom(.2, 1, .7) 
qbinom(.3, 1, .7) 
qbinom(.31, 1, .7)

# pair of independent normal random variables
mux = muy = 0
sigmax = sigmay = 1/10
f_ind = function(x, y) dnorm(x, mux, sigmax)*dnorm(y, muy, sigmay)
y = x = seq(-0.4, 0.4, length= 100) # grid to plot
z_ind = outer(x, y, Vectorize(f_ind) )
persp(x,y,z_ind, theta = -45, phi = 45, expand = 0.5, col = "lightblue", ticktype="detailed")

library(mvtnorm) # multivariate normal density
sigmaxy = 0.005
covmat = matrix(c(sigmax^2, sigmaxy, sigmaxy, sigmay^2), nrow=2, byrow=TRUE)
covmat
f_dep = function(x, y) dmvnorm( c(x,y), c(mux, muy), covmat)
# sample values over the grid
z_dep = outer(x, y, Vectorize(f_dep))
# perspective Plot
persp(x,y,z_dep, theta = -45, phi = 45, expand = 0.5, col = "lightblue", ticktype="detailed")

# another library
library(plot3D)
hist3D(z=z_dep, ticktype="detailed", space=0.1, shade=0.5)
image2D(z_dep)
image2D(z_ind)

#### Sum of random variables

# E(n,l) = X_1 + ... + X_n where X_i ~ Exp(l) are i.i.d.
# Gam(n, l) generalize E(n, l) to non-integer n

# from sds06.R: Euler's gamma(x) function extends factorial(x-1)
plot(1:10, factorial(1:10), log="y")
curve(gamma(x+1), add=T, col=2)

# Gamma distribution
curve(dexp(x, 1), ylim=c(0,1), xlim=c(0, 10)) # lambda = 1
curve(dgamma(x, 1, 1), add=T, col=2)
curve(dgamma(x, 2, 1), add=T, col=3)
curve(dgamma(x, 5, 1), add=T, col=4)
