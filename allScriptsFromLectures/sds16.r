######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 16: Numerical summaries ##########

par(mar=c(4,4,1,1))

x = faithful$eruptions
n = length(x)
mean(x)
median(x)
var(x) # notice: divided by n-1
sum((x-mean(x))^2)/(n-1)
sd(x)
median(abs(x - median(x))) # explicit MAD
mad(x, constant=1) # built-in
quantile(x, 0.1) # see help for different types
quantile(x, 0.9)
quantile(x, 0.75)-quantile(x,0.25) # inter-quartile range: upper - lower quartile
IQR(x) # built-in
# basic summaries
summary(x)

# mean vs median: robustness wrt outliers
f = function(n) {
  rnd = c(rnorm(n), c(100)*sample(1:10, 1) )
  c(mean(rnd), median(rnd)) # paired comparison
}
x = seq(100,10000,100)
yz = sapply(x, f)
plot(x, yz[1,], ylim=c(-1, 1)) # mean
abline(h=0, col=2)
points(x, yz[2,], col=3, pch=2) # median
sd(yz[1,])
sd(yz[2,]) # smaller variability for median!

# mean vs median: robustness wrt large noise
f = function(n) {
  rnd = rnorm(n) + rnorm(n, 0, 10)
  c(mean(rnd), median(rnd)) # paired comparison
}
yz = sapply(x, f)
plot(x, yz[1,], ylim=c(-1,1)) # mean
abline(h=0, col=2)
points(x, yz[2,], col=3, pch=2) # median
sd(yz[1,])
sd(yz[2,]) # larger variability for median!

# sd vs mad: robustness wrt outliers
f = function(n) {
  rnd = c(rnorm(n), c(100)*sample(1:10, 1) )
  c(sd(rnd), mad(rnd, constant=1))
}
yz = sapply(x, f)
plot(x, yz[1,], ylim=c(0.1,100), log="y") # sd
abline(h=1, col=2)
points(x, yz[2,], col=3, pch=2) # mad
sd(yz[1,])
sd(yz[2,]) # smaller variability

# quantile types
dataset = c(2,3,4,5,6)
p = seq(0, 1, 0.01)
plot(p, quantile(dataset, p, type=7), type='l', ylab='q')
points(p, quantile(dataset, p, type=6), type='l', col=2)

# small difference for large datasets
dataset = 2:200
plot(p, quantile(dataset, p, type=7), type='l', ylab='q')
points(p, quantile(dataset, p, type=6), type='l', col=3)

# Tukeys' box and whiskers
attach(iris)
boxplot(Sepal.Width)
summary(Sepal.Width)
boxplot(Sepal.Width ~ Species) # conditional
# positive skeweness
boxplot(rexp(1000)) 

# sample correlation coefficients
x = rnorm(1000); y = rnorm(1000); plot(x,y) # independent
cor(x,y, method="pearson") # uncorrelated
x = rnorm(1000, 10, 1); y = x+runif(1000, 0, 1); plot(x,y) # linearly dependent
r = cor(x,y, method="pearson") # linearly correlated
r
abline(a=0.5754, b=r*sd(y)/sd(x), col=2, lwd=2) # related to regression slope

# X ~ U(0,1 ) -> -log(X) ~ Exp(1)
x = runif(1000); y = -log(x) + rnorm(1000, 0, 0.1)
plot(x, y)
curve(-log(x), add=T, col=2, lwd=2)
cor(x,y, method="pearson") # correlated, but not linearly
cor(x,y, method="spearman") # monotonically correlated
plot(rank(x), rank(y))
cor(rank(x),rank(y), method="pearson") # Spearman's is Pearson's over the ranks!
cor(x,y, method="kendall") # this is tau.b
library(VGAM)
kendall.tau(x, y) # this is tau.a

# a binary classifier 
library(caret) # we'll cover more in future lessons
# train
x = runif(1000); y = as.factor(round(x))
x = x + rnorm(1000, 0, 0.2) # add noise
df_train = data.frame(x, y)
lr.fit = train(y~x, data=df_train, method = "glm", family="binomial") # logistic regression
# test
x = runif(200); y = as.factor(round(x))
x = x + rnorm(200, 0, 0.2) # add noise
df_test = data.frame(x, y)
lr.prob = predict(lr.fit, newdata = df_test, type="prob") # as predict_proba in scikit-learn
View(lr.prob)
lr.pconf = lr.prob[,2] # scores
# quality measures
library(ROCR)
lr.prediction = prediction(lr.pconf, df_test$y)
lr.roc = performance(lr.prediction, "tpr", "fpr")
plot(lr.roc, colorize=T, lwd=2); abline(a=0, b= 1) # ROC plot
# sort by lr.pconf decreasing
y_ord = as.numeric(y[order(lr.pconf, decreasing=T)])-1
tpr = cumsum(y_ord)/sum(y_ord)
xv = seq(0, 1, length.out=length(y_ord))
plot(xv, tpr, type='l', lwd=2) # CAP plot
kendall.tau(lr.pconf, y) # this is tau.a
d = kendall.tau(lr.pconf, y)/kendall.tau(y, y) # Somers'D or Gini
d
d/2+0.5 # auc
# AUC: performance returns an object whose members (called 'slots') are accessed with '@'
o = performance(lr.prediction,"auc")
o@y.values[[1]]
