######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 21: Regression methods #######

par(mar=c(4,4,1,1))

# load dataset
library(SemiPar)
data(janka)
x = janka$dens
y = janka$hardness

# scatter plot: pch is type of points, cex is their size
plot(x, y, xlab='dens', ylab='hadness', xlim=c(20, 80), ylim=c(0,3500), pch=16, cex=0.8)

# simple linear model
fit = lm(y~x)
fit
abline(fit, col='red', lw=2) # regression line

# summary info
summary(fit)

# fitted values
fit_ys = fitted(fit) 
res = resid(fit) #  same as y - fit_ys
# standard errors
n = length(x)
s.hat = sqrt(sum(res^2)/(n-2))
xbar = mean(x)
SXX = sum((x-xbar)^2) # or var(x)*(n-1)

# prediction on new data +- errs
x_test = data.frame(x = seq(-500, 500, 20))
# predict requires a dataframe in input
y_pred = predict(fit, x_test)
plot(x_test$x, y_pred)
errs = s.hat*sqrt(1/n+ (mean(x)-x_test$x)^2/SXX)
segments(x_test$x, y_pred-errs, x_test$x, y_pred+errs, col=2, lwd=2)
lines(x_test$x, y_pred-errs, col=3, lwd=1)
lines(x_test$x, y_pred+errs, col=3, lwd=1)

# Weighted Least Square
library(GLMsData)
data(gestation)
# Age in weeks of gestation 
# Weight: weight at birth
# Births: count of births - i.e, weighting of cases
View(gestation)
plot(Weight~Age, data=gestation)
fit = lm(Weight~Age, data=gestation)
summary(fit)
abline(fit, col=2, lwd=2)
# replicate by number of births
x = rep(gestation$Age, gestation$Births)
y = rep(gestation$Weight, gestation$Births)
# plot(x, y) # the same as before
fit = lm(y~x)
abline(fit, col=3, lwd=2)
summary(fit)
# or weighted form of linear regression
fit = lm(Weight~Age, data=gestation, weights=Births)
summary(fit)

# polynomial regression
# generate dataset
set.seed(0)
x = seq(from=0, to=20, by=0.1)
f = function(x) 500 + 0.4 * (x-5)^3
y = f(x)
plot(x, y)
noise = rnorm(length(x), mean=0, sd=40)
noisy.y = y + noise
plot(x, noisy.y, col='blue', xlab='x')
lines(x, y, col='red', lwd=2)
# polynomial regression
fit = lm( noisy.y ~ poly(x,3) )
summary(fit)
# fitted vs residuals
plot(fitted(fit), resid(fit), xlab="fitted", ylab="residuals") # fiited vs residuals
# visual fit
plot(x, noisy.y, col='blue', xlab='x')
lines(x, y, col='red', lwd=2)
lines(x, fitted(fit), col='green', lwd=2)
legend("topleft",c("true","predicted"), col=c("red","green"), lwd=3)

# non-linear regression
# generate Zipfian population
alpha = 2.5
C = 100000
# sample
x = sample(1:1000, 200)
y = C*x^{-alpha}
plot(x, y, log="xy", col='blue', xlab='x')
# and multiply by noise
noise = rnorm(length(x), 0, 0.8)
noisy.y = y*exp(noise)
plot(x, noisy.y, log="xy", col='blue', xlab='x')
curve(C*x^{-alpha}, col=2, lwd=2, add=T)
# fitting using linear regression over log-scaled data
fit1 = lm( log(noisy.y) ~ log(x))
# cannot use abline!
lines(x, exp(fitted(fit1)), col='green', lwd=2) 
summary(fit1)
intercept = exp(coef(fit1)[1])
slope = coef(fit1)[2]
cat('estimate C', intercept)
cat('estimate -alpha', slope)
# fitting using non-linear regression (favors fitting top ranks)
# initial guess required: start=list(c=intercept, b=coefficient)
fit2 = nls(noisy.y ~ c*x^b, start=list(c=intercept, b=slope))
summary(fit2)
cat('estimate C', coef(fit2)[1])
cat('estimate -alpha', coef(fit2)[2])
lines(x, fitted(fit2), col='black', lwd=2)
legend("topright",c("true","fit1-lm", "fit2-nls"), col=c("red","green","black"), lwd=3)
# fitted vs residuals
plot(fitted(fit1), resid(fit1), xlab="fitted", ylab="residuals") 
# nls assume additive noise
plot(fitted(fit2), resid(fit2), xlab="fitted", log="xy", ylab="residuals") 

# multiple (variable) linear regression
library(ISLR)
data(Carseats)
View(Carseats)
fit = lm(Sales~Advertising+Price, data=Carseats)
summary(fit)
# dummy encoding for factors (categorial)
fit2 = lm(Sales~Advertising+Price+ShelveLoc, data=Carseats)
summary(fit2)

# Multivariate regression
ami_data = read.table("http://static.lib.virginia.edu/statlab/materials/data/ami_data.DAT")
names(ami_data) = c("TOT","AMI","GEN","AMT","PR","DIAP","QRS")
# independent variables
# GEN, gender (male = 0, female = 1)
# AMT, amount of drug taken at time of overdose
# PR, PR wave measurement
# DIAP, diastolic blood pressure
# QRS, QRS wave measurement
# dependent variables
# TOT is total TCAD plasma level
# AMI is the amount of amitriptyline present in the TCAD plasma level.
View(ami_data)
fit = lm(cbind(TOT, AMI) ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
summary(fit)
coef(fit)

# omitted variable bias
n = 100
age = runif(n, 18, 70)
experience = age + runif(n, -3, 3) # x and z correlated
cor(age, experience, method='pearson')
productivity = 2*age + 3*experience + rnorm(n)
lm(productivity~age+experience) # good!
lm(productivity~age) # omitted variable bias!

# multiple (variable) linear regression
library(MASS)
library(car)
data(Boston)
# medv is median value of owner-occupied homes in $1000s.
View(Boston)

# regression from all other variables
fit = lm(medv ~ ., data = Boston)
summary(fit)
# highly correlated variables -> multi-collinearity
d=cor(Boston)
d[abs(d) < 0.8] = NA
View(d)
# variance inflation factors (cutoff at 5)
vif(fit)
# regression from all other variables except tax
fit2 = lm(medv ~. -tax, data = Boston)
summary(fit2)
vif(fit2)

# variable selection: stepwise regression 
step = stepAIC(fit, direction="backward")
summary(step)

# penalized multiple linear regression
library(penalized) # https://cran.r-project.org/web/packages/penalized/penalized.pdf
# RIDGE: lambda2>0 and lambda1=0
# LASSO: lambda2=0 and lambda1>0
# ELASTIC NET lambda1>0 and lambda2>0
fit = penalized(medv ~. , data = Boston, lambda2=1) # Ridge
print(fit)
coef(fit)
abs(coef(fit)) < 0.01
fit = penalized(medv ~. , data = Boston, lambda1=1) # Lasso
print(fit)
coef(fit)
abs(coef(fit)) < 0.01

# Michelin dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mich = read.csv('sds21.MichelinNY.csv')
View(mich)
attach(mich)
# cannot do linear regression inMichelin = alpha + beta rating
plot(Food, InMichelin)
fit = lm(InMichelin~Food)
abline(fit, col=2, lwd=2)
summary(fit) # small R-squared!
# can do linear regression on P(InMichelin) = alpha + beta Food
agg = tapply(InMichelin, Food, mean)
agg
rating = as.numeric(names(agg)) # Food ratings
rating
f = unname(agg) # P(InMichelin=1)
f
plot(rating, f) # S-shaped
fit = lm(f~rating)
summary(fit) # better R-squared!
abline(fit, col=2, lwd=2)
# can do better linear regression on logit(p) = alpha + beta rating
# where logit(x) = log(x/(1-x)) is the log of odds x/(1-x)
library(car) # for logit
library(boot) # for inv.logit
logit_f = car::logit(f, adjust=0.025) # adjust for 0 or 1 probabilities 
plot(f, logit_f)
curve(logit(x), col=2, lwd=2, add=T)
plot(rating, logit_f)
fit2 = lm(logit_f~rating)
summary(fit2) # even better R-squared!
abline(fit2, col=2, lwd=2)
# plot over p
plot(rating, f)
lines(rating, inv.logit(fitted(fit2)), col='red', lwd=2)
lines(rating, fitted(fit), col='blue', lwd=2)

# Generalized Linear Models
# family = error_distribution("link function")
plot(Food, InMichelin)
fit = glm(InMichelin~Food, data=mich, family=binomial("logit"))
points(Food, fitted(fit), col='red', lwd=2)
summary(fit)

fit = glm(formula=InMichelin~Food+Decor+Service+Price,
          family=binomial("logit"), data=mich)
summary(fit)

# Elastic net logistic regression with cross-validation
library(glmnet)

x = model.matrix(InMichelin~Food+Decor+Service+Price, mich)[,-1]
View(x)
y = mich$InMichelin
fit.lasso = glmnet(x, y, alpha=1, family = "binomial")
coef(fit.lasso)
# cross-validation search of lambda
cv.lasso = cv.glmnet(x, y, alpha = 1, family = "binomial")
cv.lasso$lambda.min # best lambda
cv.lasso$lambda.1se # good lambda and simplest model
# Fit the final model on the training data
model = glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.1se)
# Display regression coefficients
coef(model)
