######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 19:  MLE

par(mar=c(4,4,1,1))

# The German Tank exercise
t12.est = function(data) 
{
  n = length(data)
  t1 = 2*mean(data)-1
  t2 = (n+1)/n*max(data)-1
  #print(c(t1, t2))
  return( c(t1, t2) )
}

N = 1000 # tanks produced
n = 100 # tanks destroyed
data = replicate(2000, t12.est(sample(1:N, n)))
# [T]  textbook: Fig. 20.1
plot(density(data[2,]), col="red", main="Tanks' estimators", xlim=c(N*0.8, N*1.2))
lines(density(data[1,]), col="blue")
legend('topleft', pch=16, c('T2', 'T1'), col=c('red', 'blue'))

# data from Table 21.1
smokers = c(29,16,17,4,3,9,4,5,1,1,1,3,7)
nonsmokers = c(198,107,55,38,18,22,7,9,5,3,6,6,12)

# likelihood
L_p = function(p, data) {
  n = length(data)
  # data[i] is frequency at i for i < n
  # data[n] is frequency at >=n
  k = 1:(n-1)
  return( prod( (p*(1-p)^{k-1})^{data[-n]} ) * ((1-p)^{n-1})^{data[n]} )
}

# plots
xs = seq(0.001, 0.999, 0.001)
plot(xs, sapply(xs, FUN=function(x) L_p(x, smokers)), type='o')
# too small numbers for nonsmokers
points(xs, sapply(xs, FUN=function(x) L_p(x, nonsmokers)), type='o', col=2)

# log-likelihood
Log_p = function(p, data) {
  n = length(data)
  # data[i] is frequency at i for i < n
  # data[n] is frequency at >=n
  k = 1:(n-1)
  return( sum( (log(p)+log(1-p)*{k-1})*{data[-n]} ) + log(1-p)*{n-1}*{data[n]} )
}

# plot
plot(xs, sapply(xs, FUN=function(x) Log_p(x, smokers)), type='o')
points(xs, sapply(xs, FUN=function(x) Log_p(x, nonsmokers)), type='o', col=2)
# max (log-)likelihood (grid search)
which.max(sapply(xs, FUN=function(x) Log_p(x, smokers)))*0.001
which.max(sapply(xs, FUN=function(x) Log_p(x, nonsmokers)))*0.001

# better way with numeric search
library(stats)
# optimize(fun, interval, params, tolerance)
oest = optimize(Log_p, c(0, 1), data=smokers, tol=0.0001, maximum=T)
oest$maximum

# better way with mle(nLL, init_value), more general but requires init_value
library(stats4)
# negative log-likelihood (to be minimized)
nLL = function(p) -Log_p(p, smokers)
mest = mle(nLL, start=list(p=0.1))
mest

# MLE for normally distributed data
n = 10000
data= rnorm(n, 5, 2)
plot(density(data))

# we use mle because optimize works for 1 parameter only
nLL = function(mu, sigma) -sum(log(dnorm(data, mu, sigma)))
mest = mle(nLL, start=list(mu=mean(data), sigma=sd(data)))
mest
# start values must be sufficiently accurate, otherwise ...
mle(nLL, start=list(mu=0, sigma=1))

# for many distributions, we can rely on libraries
library(fitdistrplus)
fw = fitdist(data, "norm")
summary(fw)
plot(fw)

# Reported fw$loglik is computed at the maximum
-nLL(fw$estimate[1], fw$estimate[2])

# The Akaike information criterion (AIC) 
# AIC = 2*nparam -2*LL
2*2 -2*fw$loglik

# The Bayesian information criterion (BIC)
# BIC = nparam*log(ndata) -2*LL
2*log(n) -2*fw$loglik

### Recall from sds11.R
### cross-entropy and KL divergence
data = rgeom(1000, 0.7) 
px = table(data)/length(data) # empirical distribution

kldist = function(p) {
  # theoretical distribution
  values = as.numeric(names(px))
  py = dgeom(values, p) 
  # KL calculation
  cross_entropy = -sum(px*log(py))
  entropy = -sum(px*log(px))
  kl = cross_entropy - entropy
  kl
}

p = seq(0.6, 0.8, 0.005)
plot(p, sapply(p, kldist)) # minimize kl iff minimize cross_entropy

# cross_entropy between empirical and theoretical is nLL
fw = fitdist(data, "geom")
summary(fw)
plot(fw)
