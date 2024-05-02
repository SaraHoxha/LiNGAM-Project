######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 27: Bootstrap ##########

par(mar=c(4,4,1,1))

# U(0, theta)
theta = 6
mu=theta/2 # expectation of U(0, theta)
n=100 # sample size

# if we could generate a lot of datasets ...
# fit the distribution of empirical estimates
emp = replicate(1000, mean(runif(n, 0, theta)))
plot(density(emp), main='')
# CLT - true distribution (for n->infty)
curve(dnorm(x, mu, theta/sqrt(12)/sqrt(n)), add=T, col="red")

# if we couldn't generate datasets, use bootstrap
data = runif(n, 0, theta) # single dataset
xbar = mean(data)
# sampling with replacement
xbarstar = replicate(1000, mean(sample(data, n, replace=TRUE)))
deltastar = xbarstar - xbar 
plot(density(deltastar), main='')
# CLT - true distribution (for n->infty)
curve(dnorm(x, 0, theta/sqrt(12)/sqrt(n)), add=T, col="red") 
# estimates of mu
cat('Point estimate', xbar, 'Bootstrap estimate', xbar - mean(deltastar))
cat('Estimated bias', mean(deltastar), 'or', mean(xbarstar)-xbar)
cat('Std err', sd(data)/sqrt(n) ,'Bootstrap std error', sd(deltastar))

# 95% mass of deltastar
d = quantile(deltastar, c(.025, .975))
# hence: mu \in [xbar-max, xbar-min]
ci = xbar - unname(c(d[2], d[1]))
# confidence interval
ci
# equivalent calculation (found in some other texts)
d1 = quantile(xbarstar, c(.025, .975))
ci1 = 2*xbar - unname(c(d1[2], d1[1]))
ci1

# Confidence interval for the mean
# Bootstrap confidence interval 
library(boot) # boot library
# return mean of a subset d of data at positions pos
mean.pos = function(d, pos) mean(d[pos])
# sample usage of mean.pos
mean(data[1:5])
mean.pos(data, 1:5)
# calling boot()
b = boot(data, mean.pos, R=1000)
plot(b)
b
b$t0 # original = estimator on all data, i.e., xbar
xbar
head(b$t) # bootstrap estimates
deltastar = b$t-b$t0 # b$t-b$t0 is deltastar
mean(deltastar) # bias is mean(b$t)-b$t0
sd(deltastar) # std. error is sd(b$t), or sd(deltastar)

plot(density(deltastar)) 
# CLT on centered distribution
curve(dnorm(x, 0, 2*mu/sqrt(12)/sqrt(n)), add=T, col="red") 
# worse approximation when working on non-centered 
# see Remark 18.1 of the textbook
plot(density(b$t)) 
# CLT distribution
curve(dnorm(x, mu, 2*mu/sqrt(12)/sqrt(n)), add=T, col="red") 

plot(density(deltastar)) 
# 95% mass
d = quantile(deltastar, c(0.025, 0.975))
ci = b$t0 - unname(c(d[2], d[1]))
# confidence interval (based on Empirical bootstrap)
ci

# Empirical bootstrap confidence intervals using the boot library
bci = boot.ci(b, conf=0.95, type="basic") # conf = 0.95 is default
bci
bci$basic[4:5] 
ci # small differences (due to different quantile interpolation)

# CI based on percentiles
bci = boot.ci(b, conf=0.95, type="perc")
bci
bci$percent[4:5] 
quantile(b$t, c(0.025, 0.975)) # small differences (due to different quantile interpolation)

# CI based on normal fitting
bci = boot.ci(b, conf=0.95, type="norm")
bci
bci$normal[2:3]
# explicit calculation
d = qnorm(c(0.025, 0.975), mean(deltastar), sd(deltastar))
ci = b$t0 - unname(c(d[2], d[1]))
ci

# CI based on bias correction and accelleration (best method)
bci = boot.ci(b, conf=0.95, type="bca")
bci
bci$bca[4:5]

# CI based on studentized fitting (see also textbook [T, section 23.3])
# calling boot() with mean and var estimators
mean.var.pos = function(d, pos) c(mean(d[pos]), var(d[pos])/length(pos))
b = boot(data, mean.var.pos, R=1000)
bci = boot.ci(b, conf=0.95, type="stud")
bci
bci$student[4:5]
# explicit calculation
tstar = (b$t[,1] - b$t0[1])/sqrt(b$t[,2])
plot(density(tstar))
curve(dt(x, n-1), add=T, col=2)
d = quantile(tstar, c(0.025, 0.975)) 
ci = b$t0[1] - unname(c(d[2], d[1]))*sqrt(b$t0[2])
ci # small differences (due to different quantile interpolation)

# large sample vs bootstrap
library(BSDA) # for z.test

# compute size of large sample and bootstrap confidence intervals
# 95% conf.level is default parameter 
cicmp = function(data, truemu) {
  # large sample confidence interval
  ls_int = z.test(data, sigma.x=sd(data))$conf.int
  ls_ok = (ls_int[1] <= truemu & truemu <= ls_int[2])
  # bca type. Try other types.
  b = boot(data, mean.pos, R=1000) 
  # boostrap confidence interval
  bs_int = boot.ci(b, type="bca")$bca[4:5]
  bs_ok = (bs_int[1] <= truemu & truemu <= bs_int[2])
  return( c( diff(ls_int), diff(bs_int), ls_ok, bs_ok) )
}

# compare large sample and bootstrap confidence intervals
n = 20 # also try with larger n
# uniform distribution
data = replicate(1000, cicmp( runif(n, 0, theta), mu ) )
# data is a matrix 4x1000
View(data)
rowMeans(data) # or apply(data, 1, mean)
plot( density(data[1,]), xlab="CI width", main="runif" )
lines( density(data[2,]), col="red" )
legend("topright",c("large sample", "bootstrap"), col=c("black", "red"), lwd=3)
# normal distribution
data = replicate(1000, cicmp( rnorm(n, mu), mu ) )
rowMeans(data) # or apply(data, 1, mean)
# exponential distribution
lambda=1; mu = 1/lambda
data = replicate(1000, cicmp( rexp(n, lambda), mu ) )
rowMeans(data) # or apply(data, 1, mean)
# power-law
library(poweRlaw)
alpha = 3.1; mu = (alpha-1)/(alpha-2) # alpha > 3 => mu and sigma^2 exist
data = replicate(1000, cicmp( rplcon(n, 1, alpha), mu ) )
rowMeans(data) # or apply(data, 1, mean)

# Bootstrap for a generic estimator!
# Example: The German Tank exercise
N = 1000 # tanks produced
n = 100 # tanks destroyed
# t2 estimator based on max
t2.est = function(data, positions) {
  n = length(positions)
  return( (n+1)/n*max(data[positions])-1 )
}
#
data = sample(1:N, n) # sample
b = boot(data, t2.est, R=1000)
plot(b) # not normal!
b$t0
# confidence interval
boot.ci(b, type="bca")$bca[4:5]

# Confidence intervals for linear regression coefficients
# based on bootstrap
plot(catsM$Bwt, catsM$Hwt)
fit = lm( Hwt~Bwt, data=catsM ) # linear regression
abline(fit, col='red', lw=2) # regression line
cats.fit = function(data) {
  fit = lm( data$Hwt~data$Bwt )
  coef(fit)
}
case.fun = function(d,pos) cats.fit(d[pos,])
b = boot(catsM, case.fun, R=1000)
b
plot(density(b$t[,1])) # density of bootstrap intercept
plot(density(b$t[,2])) # density of bootstrap coefficient/slope
cat('95% CI for intercept')
boot.ci(b, type="bca", index=1)$bca[4:5]
cat('95% CI for coefficient/slope')
boot.ci(b, type="bca", index=2)$bca[4:5]
# vs normality assumption
confint(fit) 

# Conf Int means P(L < mu < U) = 95%
# Bootstrap for probability estimation
# E.g., P(|Xn - mu| > 1) ?
n = 100; mu=0; sigma=10 
data = rnorm(n, mu, sigma)
b = boot(data, mean.pos, R=1000) # try R=10000
# estimated probability
mean(abs(b$t - b$t0) > 1)
# true probability
# P(Xn > mu + 1) + P(Xn< mu-1) where Xn ~ N(mu, sigma^2/n)
(1-pnorm(mu+1, mu, sigma/sqrt(n))) + pnorm(mu-1, mu, sigma/sqrt(n))


# Resampling for classifier performance estimation
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
#### adult dataset from sdsR17.R
# execute script in a separate file
#source("dataprep.R", echo=T) 
# and save in R binary format
#save(data, file="sds17.Rdata") 

# or load previously saved data
load(file="sds17.Rdata")

# using caret (Classification and Regression Training)
# https://topepo.github.io/caret/available-models.html
library(caret)

# training 80% holdout set 20%
# LGOCV (Leave-group-out-CV = repeated train/test split) 
r1 = trainControl(method="LGOCV", number =1, p=0.8, savePredictions=T)
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", family=binomial(logit), trControl=r1)
lr.fit$pred
confusionMatrix(lr.fit$pred$pred, lr.fit$pred$obs, positive="1")

# cross-validation
r2 = trainControl(method="cv", number=10, savePredictions=T)
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", family=binomial(logit), trControl=r2)
lr.fit
lr.fit$resample
lr.fit$results
mean(lr.fit$resample$Accuracy) # Accuracy
sd(lr.fit$resample$Accuracy) # AccuracySD
# Wald CI using Accuracy and AccuracySD 
z = qnorm(1-0.05/2)
lr.fit$results$Accuracy+lr.fit$results$AccuracySD*c(-z,z)
# CI for proportions
se = sqrt(lr.fit$results$Accuracy*(1-lr.fit$results$Accuracy)/nrow(data$train))
lr.fit$results$Accuracy+se*c(-z,z)
# CI for proportions are used in the confusionMatrix!
confusionMatrix(lr.fit$pred$pred, lr.fit$pred$obs, positive="1") # small differences (due to different quantile interpolation)

# bootstrap
r3 = trainControl(method="boot632", number=25, savePredictions=T)
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", family=binomial(logit), trControl=r3,
               metric="Accuracy")
lr.fit
lr.fit$resample
lr.fit$results
# Wald CI using Accuracy and AccuracySD
z = qnorm(1-0.05/2)
lr.fit$results$Accuracy+lr.fit$results$AccuracySD*c(-z,z)
# CI using bootstrap quantiles
plot(density(lr.fit$resample$Accuracy))
unname(quantile(lr.fit$resample$Accuracy, c(0.025, 0.975)))

# repeated cross-validation
r4 = trainControl(method="repeatedcv", repeats=3, number=10, savePredictions=T)
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", family=binomial(logit), trControl=r4)
lr.fit
lr.fit$resample
lr.fit$results
# Wald CI using Accuracy and AccuracySD
z = qnorm(1-0.05/2)
lr.fit$results$Accuracy+lr.fit$results$AccuracySD*c(-z,z)


## Parametric Bootstrap 

par(mar=c(4,4,1,1))

library(boot) 

# U(0, theta)
theta = 6
mu=theta/2 # expectation of U(0, theta)
n=100 # sample size
data = runif(n, 0, theta) # dataset
xbar = mean(data)

# assume to know that data is from a X~U(0, theta), with mu = E[X] = theta/2
# estimator mean of E[X], and then, theta = 2*mu

# parametric bootstrap
muhat = mean(data) # estimate of mu
b = boot(data, # dataset
         mean, # estimator
         R=1000, # repetitions
         sim = "parametric", 
         ran.gen = function(d, par) runif(length(d), 0, 2*par),
         mle = muhat # distribution parameter
        )
b
boot.ci(b, type="basic") # bca not available for parametric bootstrap

# Parametric bootstrap: CI of theat for X~Bin(15, theta)
theta = 0.9 # true (but unknown) parameter 
binomSize = 15 # known size of binomial
n = 20 # sample size, the larger the better
data = rbinom(n, binomSize, theta) # dataset
thetahat = mean(data)/binomSize # MLE estimate for theta (Exercise: prove it!)
nboot = 1000 # number of bootstrap samples to use

# using boot
b = boot(data, function(d) mean(d)/binomSize, 
         R=nboot, sim = "parametric", 
         ran.gen = function(d, par) rbinom(length(d), binomSize, par),
         mle = thetahat)
b
boot.ci(b, type="basic") # slightly different because quantile calculations

# manual calculation
tmpdata = rbinom(n*nboot, binomSize, thetahat) # nboot parametric samples of size n
bootstrapsample = matrix(tmpdata, nrow=n, ncol=nboot) # organize in a matrix
View(bootstrapsample)
# Compute bootstrap MLE's thetahat* and differences delta*
thetahatstar = colMeans(bootstrapsample)/binomSize
deltastar = thetahatstar - thetahat
plot(density(deltastar), main='')
# Find quantiles and make the bootstrap confidence interval
d = quantile(deltastar, c(.025,.975))
ci = thetahat - unname(c(d[2], d[1]))
ci


# Parametric bootstrap: are the software data exponential?
# software data (interfailure time) 
# from textbook [T, Table 15.3, page 218]
swdata = c(30,113,81,115,9,2,91,112,15,138,50,77,24,108,88,670,120,26,114,325,55,242,68,422,180,10,1146,600,15,36,4,0,8,227,65,176,58,457,300,97,263,452,255,197,193,6,79,816,1351,148,21,233,134,357,193,236,31,369,748,0,232,330,365,1222,543,10,16,529,379,44,129,810,290,300,529,281,160,828,1011,445,296,1755,1064,1783,860,983,707,33,868,724,2323,2930,1461,843,12,261,1800,865,1435,30,143,108,0,3110,1247,943,700,875,245,729,1897,447,386,446,122,990,948,1082,22,75,482,5509,100,10,1071,371,790,6150,3321,1045,648,5485,1160,1864,4116)
# pdf
hist(swdata, prob=TRUE)
lambdahat = 1/mean(swdata)
curve(dexp(x, lambdahat), col=2, add=T, lwd=2)
# cdf
cdf = ecdf(swdata)
plot(cdf)
curve(pexp(x, lambdahat), col=2, add=T, lw=2)

# Kolmogorov-Smirnov distance between a sample and a distribution
ks.dist = function(sample, distribution) {
  cdf = ecdf(sample) 
  u = unique(sample)
  dist = max(abs(cdf(u) - distribution(u)))
  v = u[which( abs(cdf(u) - distribution(u)) == dist)]
  return(c(dist, v))
}
# exercise: write ks.dist(sample1, sample2) for 2 empirical samples

# KS distance between swdata and Exp(lambda)
dv = ks.dist(swdata, function(x) pexp(x, lambdahat))
dv
segments(dv[2], cdf(dv[2]), dv[2], pexp(dv[2], lambdahat), col="blue", lw=2) # zoom 

# bootstrap test of hypothesis
b = boot(swdata, function(d) ks.dist(d, function(x) pexp(x, 1/mean(d)))[1], 
         R=1000, sim = "parametric", 
         ran.gen = function(d, par) rexp(length(d), par),
         mle = lambdahat) # H0 is X~Exp(lambdahat) 
b
plot(b)
plot(density(b$t)) # density of bootstrap t_{ks}
1-ecdf(b$t)(0.17) # P(T_{ks} > 0.17)
mean(b$t > 0.17) # alternative P(T_{ks} > 0.17)
boot.ci(b, type="perc") # 95% confidence interval for T_{ks}
