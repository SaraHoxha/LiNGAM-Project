######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 29: Test of hypothesis ##########

par(mar=c(4,4,1,1))

# z-Test of the mean
# Normal data, known variance
test.z = function(data, mu0, sigma, conf.level=0.95)
{ # extends ci.z from smd17.R
  n = length(data)
  xn = mean(data)
  alpha = 1 - conf.level
  # P(Z >= cu) = alpha/2 <=> P(Z <= cu) = 1-alpha/2
  cu = qnorm(1-alpha/2) # cu is the (1-alpha/2) quantile
  delta = cu*sigma/sqrt(n)
  cat("mean", xn, "delta", delta)
  cat(paste0("\n",conf.level*100, "% CI ("), c(xn-delta, xn+delta), ")" )
  # right-sided interval
  # P(Z >= cv) = alpha <=> P(Z <= cv) = 1-alpha
  cu1 = qnorm(1-alpha) 
  delta1 = cu1*sigma/sqrt(n)
  cat(paste0("\n",conf.level*100, "% one-sided CI ("), xn-delta1, "Inf )" )
  z.value = (xn - mu0)/sigma*sqrt(n)
  p = 1-pnorm(abs(z.value)) # P(Z >= |z.value|)
  cat("\nmu0 =", mu0, "z =", z.value, "\ntwo-sided p-value =", 2*p, "\none-sided p-value =", p)
}

# statistical test of the mean
n = 100; mu = 3; s = 4
data=rnorm(n, mu, s)
test.z(data, mu0=1, sigma=s);   # can be rejected at 95% confidence
test.z(data, mu0=2.5, sigma=s); # cannot be rejected

# library with z test and Confidence Intervals built-in
library(BSDA)
z.test(data, mu=2.5, sigma.x=s)
z.test(data, mu=2.5, sigma.x=s, alternative="greater")

# t-Test
# Normal data, unknown variance
test.t = function(data, mu0, conf.level=0.95)
{ # extends ci.t from smd17.R
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
  t.value = (xn - mu0)/sd(data)*sqrt(n)
  p = 1-pt(abs(t.value), n-1) # P(T >= |t.value|)
  cat("\nmu0 =", mu0, "t =", t.value, "df =", n-1, "\ntwo-sided p-value =", 2*p, "\none-sided p-value =", p)
}

# test mean = 0 - cannot be rejected
n = 100; data=rnorm(n); test.t(data, mu0=0);  
t.test(data, mu=0) # r built-in
# test mean = -0.5 - can be rejected
test.t(data, mu0=-0.5);
t.test(data, mu=-0.5) # r built-in
t.test(data, m=-0.5, alternative="greater")

### but, how to check whether data is normal?
qqnorm(data) # visually
qqline(data, col=2, lwd=2)
# Shapiro test of normality H0: X~N(mu, sigma^2) for some mu, sigma^2
help(shapiro.test)
shapiro.test(data)
shapiro.test(rexp(n))

# General data, large sample
n = 1000; data=rexp(n, rate=2) # mean = 1/rate = 0.5 
qqnorm(data); qqline(data) # visually
shapiro.test(data) # normality check fails
# use z.test with sigma = sd(data)
z.test(data, mu=0.45, sigma.x=sd(data));  
# or use t.test directly
t.test(data, mu=0.45) # since t(n) -> N(0,1) for large n

# General data, symmetric distribution
# Wilcoxon signed-rank test
wilcox.test(data, mu=0.4, conf.int = T)  # fails because exp is not symmetric!
n = 10; data=rbinom(n, size=100, prob=0.5); # mean = size*prob = 50
wilcox.test(data, mu=50, conf.int = T)
wilcox.test(data, mu=46, conf.int = T)

# General data, bootstrap t-test

goldenratio = (sqrt(5)-1)/2
goldenratio
shoshoni = c(0.693,0.749,0.654,0.670,0.662,0.672,0.615,0.606,0.690,0.628,0.668,0.611,0.606,0.609,0.601,0.553,0.570,0.844,0.576,0.933)
mean(shoshoni)
plot(density(shoshoni))
# H0: mu=goldenratio, H1: mu>goldenratio
shapiro.test(shoshoni) # cannot use t-test, nor large sample
# library with bootstrap t-test
library(MKinfer)
boot.t.test(shoshoni, mu=goldenratio, alternative="greater", R=10000) # see help
# manual calculation
n = length(shoshoni)
mean.var.pos = function(d, pos) c(mean(d[pos]), var(d[pos])/length(pos))
library(boot)
b = boot(shoshoni, mean.var.pos, R=10000)
plot(density(b$t[,1]))
bci = boot.ci(b, conf=0.9, type="stud") # conf=0.9 trick for one-sided 0.95
bci # reject H0: mu=goldenratio if goldenratio not in confidence interval
bci$student[4:5] # confidence interval
# compute p-value
tboot = (b$t[,1] - mean(shoshoni))/sqrt(b$t[,2])
plot(density(tboot), xlim=c(-6,4)) # [T] Fig. 27.2
tvalue = (mean(shoshoni) - goldenratio)/(sd(shoshoni)/sqrt(n))
tvalue
pvalue = mean(tboot >= tvalue) # one-sided P(T >= t.value)
pvalue # reject H0 in favor of H1: mu > goldenratio

# Binomial test for classifier accuracy
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
summary(lr.fit)
# predicted confidence on validation set
lr.pred = predict(lr.fit, newdata = data$val)
confusionMatrix(lr.pred, data$val$income, positive="1")
# exact binomial test
binom.test(sum(lr.pred == data$val$income), length(lr.pred), p=0.8)
# large sample approximation of binomial test
t.test(lr.pred == data$val$income, mu=0.8)

# t-test for linear regression (with Gaussian noise)
library(SemiPar)
data(janka)
attach(janka)
# scatter plot
plot(dens,hardness)
# simple linear model
fit = lm(hardness~dens)
abline(fit, col='red', lw=2) # regression line
s = summary(fit) # t-value wrt the H0: par=0
s
# explicit calculation
n = length(hardness)
se = s$coef[,2] # std error
coef(fit)/se # t-value
qt(0.975, n-2) # critical value at 0.05 (2-tailed)
# P(|T| >= |t|) = 2(1-P(T < |t|))
2*(1-pt(abs(coef(fit)/se), n-2)) # p-value (2-tailed)
