######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 26:  Confidence intervals ##########

par(mar=c(4,4,1,1))

# Confidence Interval: Normal data, known variance
ci.z = function(data, sigma, conf.level=0.95, mu=NA)
{
  n = length(data)
  xn = mean(data)
  alpha = 1 - conf.level
  # P(Z >= cu) = alpha/2 <=> P(Z <= cu) = 1-alpha/2
  cu = qnorm(1-alpha/2) # cu is the (1-alpha/2) quantile
  
  if(!is.na(mu)) { # if true mu is provided, plot it
    curve(dnorm(x), xlim=c(-4, 4))
    scaledmu = (mu-xn)/sigma # (scaled) true mu position
    segments(scaledmu, 0, scaledmu, dnorm(scaledmu), col="blue")
    segments(-cu, 0, -cu, dnorm(cu), col="red") # left cv
    segments(cu, 0, cu, dnorm(cu), col="red") # right cv
  }
  
  delta = cu*sigma/sqrt(n)
  cat("mean", xn, "delta", delta)
  cat(paste0("\n",conf.level*100, "% CI ("), c(xn-delta, xn+delta), ")" )

  # right-sided interval
  # P(Z >= cv) = alpha <=> P(Z <= cv) = 1-alpha
  cu1 = qnorm(1-alpha) 
  delta1 = cu1*sigma/sqrt(n)
  cat(paste0("\n",conf.level*100, "% one-sided CI ("), xn-delta1, "Inf )" )
}

# standard normal data N(0, 1) - try different n and conf_level
n = 50; data=rnorm(n)
ci.z(data, 1, conf.level=0.95, mu=0);  
# general normal data N(mu, s)
n=100; mu=3; s=5
data=rnorm(n, mu, s)
ci.z(data, s)  
ci.z(data, s, mu=mu) # for visualization only  
# smaller sample leads to larger delta
ci.z(data[1:30], s); 

# library with z test and Confidence Intervals built-in
library(BSDA)
z.test(data[1:30], sigma.x=s)
z.test(data[1:30], sigma.x=s, alternative="greater")

### but, how to check whether dataset is normal?
qqnorm(data) # visually
qqline(data, col=2, lwd=2)
data=rexp(n, rate=2)
qqnorm(data) # visually
# test of normality in future lessons

# Confidence Interval: Normal data, unknown variance

# t-distribution
curve(dnorm(x), xlim=c(-5,5))
curve(dt(x, 1), add=T, col="red")
curve(dt(x, 2), add=T, col="blue")
curve(dt(x, 10), add=T, col="green")
legend("topright",c("norm", "m=1", "m=2","m=10"), col=c("black", "red","blue", "green"), lwd=3)

# look at the tail (log-scale)
curve(dnorm(x), xlim=c(2,20), log="y")
curve(dt(x, 1), add=T, col="red") # heavier tails
curve(dt(x, 2), add=T, col="blue")
curve(dt(x, 10), add=T, col="green")
legend("bottomleft",c("norm", "m=1", "m=2","m=10"), col=c("black", "red","blue", "green"), lwd=3)

# Confidence Interval: normal data, unknown variance
ci.t = function(data, conf.level=0.95)
{
  n = length(data)
  xn = mean(data)
  alpha = 1 - conf.level
  # P(T >= cu) = alpha/2 <=> P(T <= cu) = 1-alpha/2
  cu = qt(1-alpha/2, n-1) # cu is the (1-alpha/2) quantile
  delta = cu*sd(data)/sqrt(n)
  cat("mean", xn, "delta", delta)
  cat(paste0("\n",conf.level*100, "% CI ("), c(xn-delta, xn+delta), ")" )
  # right-sided interval
  # P(T >= cv) = alpha <=> P(T <= cv) = 1-alpha
  cu1 = qt(1-alpha, n-1) 
  delta1 = cu1*sd(data)/sqrt(n)
  cat(paste0("\n",conf.level*100, "% one-sided CI ("), xn-delta1, "Inf )" )
}

# standard normal data
n = 5; data=rnorm(n); ci.t(data);  
# general normal data
n=100; mu=3; s=5
data=rnorm(n, mu, s)
ci.z(data, s)  
ci.t(data)
t.test(data) # r built-in
t.test(data, alternative="greater") # r built-in

# Confidence Interval: Large sample method (any data, unknown variance)

# use ci.z with sigma = sd(data)
n = 1000; data=rnorm(n); ci.z(data, sigma=sd(data));  
# compare to ci.z with known sigma
ci.z(data, sigma=1)

# return true if confidence interval is correct
f = function(n, conf.level=0.95, df=rnorm, dfpar=1, true_mu=1) {
  data = df(n, dfpar)
  ci = z.test(data, sigma.x=sd(data), conf.level=conf.level)$conf.int # large sample
  return(ci[1] <= true_mu & true_mu <= ci[2])
}

# simulations (see [T] Table 23.3)
n = 1000 # also try with n = 100 and n=1000
# normal
mu = 1
norm90 = mean(replicate(1000, f(n, 0.9, rnorm, mu, mu)))
norm95 = mean(replicate(1000, f(n, 0.95, rnorm, mu, mu)))
# exponential
lambda = 1; mu = 1/lambda
exp90 = mean(replicate(1000, f(n, 0.9, rexp, lambda, mu)))
exp95 = mean(replicate(1000, f(n, 0.95, rexp, lambda, mu)))
# accuracies
c(n, norm90, norm95, exp90, exp95)
# power-law
library(poweRlaw)
alpha = 3.1; mu = (alpha-1)/(alpha-2) # alpha > 3 => mu and sigma^2 exists
# functional needed because alpha is not second position parameter 
fpl = function(n, alpha) rplcon(n, 1, alpha) 
pl90 = mean(replicate(1000, f(n, 0.9, fpl, alpha, mu)))
pl95 = mean(replicate(1000, f(n, 0.95, fpl, alpha, mu)))
c(n, norm90, norm95, exp90, exp95, pl90, pl95)
# try 2 < alpha <= 3 0 => mu exists but sigma^2 does not

# CI for classifier accuracy
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

# logistic regression
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", family=binomial(logit))
# predicted confidence on validation set
lr.pred = predict(lr.fit, newdata = data$val)
confusionMatrix(lr.pred, data$val$income, positive="1")
# 95% CI manual calculation
acc = mean(lr.pred == data$val$income)
se = sqrt(acc*(1-acc)/length(lr.pred))
alpha = 0.05
z = qnorm(1-alpha/2) 
# CI using Normal approx
acc+se*c(-z,z)
#  CI using exact Binomial distribution
binom.test(sum(lr.pred == data$val$income), length(lr.pred))
# using DescTools
library(DescTools)
help(BinomCI)
BinomCI(sum(lr.pred == data$val$income), length(lr.pred), method="wilson")

# CI for linear regression under normality of errors
library(boot) # needed for catSM
data(catsM) #  heart and body weight of 95 male cats
plot(catsM$Bwt, catsM$Hwt)
fit = lm( Hwt~Bwt, data=catsM ) # linear regression
abline(fit, col='red', lw=2) # regression line
s = summary(fit) 
s
coef(fit) # intercept and coefficient/slope
s$coef # including se
# CI based on normality of errors
se = s$coef[,2] # std error
se
n = length(catsM$Bwt)
alpha = 0.05 # 95% confidence level
coef(fit)-qt(1-alpha/2, n-2)*se # lower bound 
coef(fit)+qt(1-alpha/2, n-2)*se # upper bound
# confidence intervals: built-in function
confint(fit, level=0.99) 

# Confidence intervals for fitted values
s.hat = sqrt(sum(resid(fit)^2)/(n-2))
sxx = sum( (catsM$Bwt-mean(catsM$Bwt))^2 )
xs = seq(1, 5, 0.1)
ys = predict(fit, data.frame(Bwt = xs))
points(xs, ys, col="red")
se_ys = s.hat*sqrt(1/n+ ((mean(catsM$Bwt)-xs)^2)/sxx)
points(xs, ys+qt(0.025, n-2)*se_ys, type="l", col="blue")
points(xs, ys-qt(0.025, n-2)*se_ys, type="l", col="blue")
# R direct way (without calculation)
plot(catsM$Bwt, catsM$Hwt, xlim=c(1, 5), ylim=c(3,23))
abline(fit, col='red', lw=2) # regression line
ys = predict(fit, data.frame(Bwt = xs), interval='confidence', conf=0.95)
ys
points(xs, ys[,2], type="l", col="blue")
points(xs, ys[,3], type="l", col="blue")
# alternative library (works also for logistic regression)
library(ciTools)
ci = add_ci(data.frame(Bwt = xs), fit, alpha = 0.05, names = c("lwr", "upr"))
View(ci)
plot(catsM$Bwt, catsM$Hwt, xlim=c(1, 5), ylim=c(3,23))
abline(fit, col='red', lw=2) # regression line
points(ci$Bwt, ci$upr, type="l", col="blue")
points(ci$Bwt, ci$lwr, type="l", col="blue")
# add prediction intervals
ys = predict(fit, data.frame(Bwt = xs), interval='prediction')
View(ys)
points(xs, ys[,2], type="l", col="green")
points(xs, ys[,3], type="l", col="green")

# Logistic regression: Michelin dataset
mich = read.csv('sds22.MichelinNY.csv')
attach(mich)
fit = glm(formula=InMichelin~Food, family=binomial("logit"), data=mich)
summary(fit)
# coefficient confidence interval
confint(fit)
# fitted confidence interval
plot(Food, InMichelin)
test_data = data.frame(Food = seq(10, 35, 0.5))
ys = predict(fit, test_data, type="response") # type="response" for probabilities
points(test_data$Food, ys, col='red', lwd=2)
ci = add_ci(test_data, fit, alpha = 0.05, names = c("lwr", "upr"))
View(ci)
points(ci$Food, ci$upr, type="l", col="blue")
points(ci$Food, ci$lwr, type="l", col="blue")
