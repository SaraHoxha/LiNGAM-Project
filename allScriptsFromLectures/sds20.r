######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 20: Linear Regression and LSE

par(mar=c(4,4,1,1))

# load dataset
library(SemiPar)
data(janka)
x = janka$dens
y = janka$hardness

# scatter plot: pch is type of points, cex is their size
plot(x, y, xlab='dens', ylab='hadness', xlim=c(20, 80), ylim=c(0,3500), pch=16, cex=0.8)

# simple linear model
fit = lm(y~x) # linear formula y~x
fit
abline(fit, col='red', lw=2) # regression line

# extract parameters
lm_pars = coef(fit) 
lm_pars
intercept = lm_pars[1]
slope = lm_pars[2]
r_intercept = round(intercept, 4) 
r_slope = round(slope, 4) 
# display equation 
mtext(bquote(y == .(r_intercept) + .(r_slope) * x),  adj=1, padj=0, col='red') 

slope
# LSE formula for slope
cov(x, y)/var(x)
# Pearson's linear correlation is scale independent
cor(x, y)
# slope vs Pearson's linear correlation
cor(x, y)*sd(y)/sd(x)
# for scaled data scale(v) = (v-mean(v))/sd(v)
scaledx = scale(x)
scaledy = scale(y)
plot(scaledx, scaledy, pch=16, cex=0.8)
fit2 = lm(scaledy~scaledx)
fit2
abline(fit2, col='red', lw=2) # regression line
# beta = Pearson's correlation coefficient
cor(scaledx, scaledy)

# Given: y = alpha + beta*x
# Is it true?: x = -alpha/beta + 1/beta*y
fit3 = lm(x~y)
fit3
1/r_slope # answer: no!
# reason: formula of slope
cov(x, y)/var(y)
r_slope*var(x)/var(y)

# plot residuals
fit_ys = fitted(fit) # same as alpha + beta*x
res = resid(fit) #  same as y - fit_ys
# fitted vs residuals - note *heteroscedasticity*
plot(fit_ys, res, xlab="fitted", ylab="residuals") 
# summary info
summary(fit)

# standard errors
n = length(x)
s.hat = sqrt(sum(res^2)/(n-2))
cat('Residual std error', s.hat, 'on', (n-2), 'degrees of freedom')
xbar = mean(x)
SXX = sum((x-xbar)^2) # or var(x)*(n-1)
se.beta.hat = s.hat/sqrt(SXX)
cat('Beta std error', se.beta.hat)
se.alpha.hat = s.hat*sqrt((1/n + xbar^2/SXX))
cat('Alpha std error', se.alpha.hat)

# summary info
summary(fit)
# coefficient of determination
r2 = 1-var(res)/var(y)
cat('R-squared', r2)
# alternative definition
r2 = var(fit_ys)/var(y)
cat('R-squared', r2)
# for simple (one independent variable) linear regression
# r^2 = pearson^2 between data and fitted
r2 = cor(y, fit_ys)^2 
cat('R-squared', r2)
# adjusted R^2 use unbiased estimators of variance
k = 2 # number of parameters (can be > 2 for multiple regression)
adj_r2 = 1-var(res)/var(y)*(n-1)/(n-k)
cat('Adjusted R-squared', adj_r2)
# diagnostic plots, see https://data.library.virginia.edu/diagnostic-plots/
plot(fit)

# Attention! Anscombe's Quartet
data(anscombe)
View(anscombe)
attach(anscombe)
plot(x1, y1);fit1 = lm(y1~x1); abline(fit1, col='red', lw=2)
plot(x2, y2);fit2 = lm(y2~x2); abline(fit2, col='red', lw=2)
plot(x3, y3);fit3 = lm(y3~x3); abline(fit3, col='red', lw=2)
plot(x4, y4);fit4 = lm(y4~x4); abline(fit4, col='red', lw=2)
coef(fit1)
coef(fit2)
coef(fit3)
coef(fit4)
# study residuals!
plot(fitted(fit1), resid(fit1), xlab="fitted", ylab="residuals") 
plot(fitted(fit2), resid(fit2), xlab="fitted", ylab="residuals") 
plot(fitted(fit3), resid(fit3), xlab="fitted", ylab="residuals") 
plot(fitted(fit4), resid(fit4), xlab="fitted", ylab="residuals") 
# leverage measures distance from mean in dependent variable space 
hatvalues(fit4) 
