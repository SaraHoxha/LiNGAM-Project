######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 34: Fitting distributions. Testing independence/association

par(mar=c(4,4,1,1))

## Goodness of fit: continuous data

# KS tests
data = rnorm(1000, 10, 2)
ks.test(data, pnorm, 10, 2)
ks.test(data, pnorm, 9.7, 2)
ks.test(data, pexp)
# energy dataset
library(ISwR)
attach(energy)
x = energy$expend
hist(x, freq = FALSE); lines(density(x), lty=2)
mean(x); sd(x)
# test x is from N(mean(x), var(x))?
shapiro.test(x)
ks.test(x, pnorm, mean(x), sd(x)) 

# Fitting by MLE using fitdistrplus
# https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf
library(fitdistrplus) 
plotdist(data, demp=T)
# Cullen and Frey graph
descdist(data, boot=1000) 
# parameter fitting for a normal family
fnorm = fitdist(data, "norm") 
fnorm
# confidence intervals for parameters
b = bootdist(fnorm, niter = 1000) 
summary(b)
# parameter fitting for a log-normal family
flognorm = fitdist(data, "lnorm") 
flognorm
# parameter fitting for a gamma family
fgamma = fitdist(data, "gamma") 
fgamma
library(actuar) # for dpareto, use actuar instead of VGAM (which runs into errors with fitdist)
# parameter fitting for a power-law family
fpareto = fitdist(data, "pareto", start=list(shape=2, scale=1)) 
fpareto
plot.legend = c("normal", "lognormal", "gamma", "pareto")
lest = list(fnorm, flognorm, fgamma, fpareto)
denscomp(lest, legendtext = plot.legend) # compare densities
cdfcomp(lest, legendtext = plot.legend) # compare cumulative
qqcomp(lest, legendtext = plot.legend) # compare quantiles
# goodness of fit
gofstat(lest, fitnames=plot.legend)
# pareto is not suitable
ks.test(data, ppareto, fpareto$estimate[1], fpareto$estimate[2])
# norm is the best fit 
ks.test(data, pnorm, fnorm$estimate[1], fnorm$estimate[2])
ks.test(data, pgamma, fgamma$estimate[1], fgamma$estimate[2])
ks.test(data, plnorm, flognorm$estimate[1], flognorm$estimate[2])
# compare log of likelihood ratio R between a pair of competing models
R = fnorm$loglik - fgamma$loglik 
R # norm better than gamma
1-pchisq(-2*R, 1) # p-value
R = fnorm$loglik - flognorm$loglik 
R # norm better than lognorm
1-pchisq(-2*R, 1) # p-value
R = flognorm$loglik - fgamma$loglik 
R # gamma better than lognorm
1-pchisq(-2*R, 1) # p-value

# See powerRlaw library for fitting, bootstrap p-value and CI, 
# and goodness of fit (KS, AIC, R, etc.) for:
# - powerlaw, lognormal, exponential, poisson

## Goodness of fit: discrete data 
size = 10; truep = 0.5; n = 100
data = rbinom(n, size=size, p=truep)
agg = table(data)
agg
bins = as.numeric(names(agg))
bins
Ni = unname(agg)
Ni # observed
estp = 0.49 # estimate, try something further
pi = dbinom(bins, size=size, p=estp)
pi = pi/sum(pi) # rescale to 1
ni = pi*n
ni # expected
df = length(bins)-1 # degrees of freedom
xi2 = sum((Ni - ni)^2/ni)
xi2
1-pchisq(xi2, df=df) # pvalue
# built-in function
chisq.test(Ni, p=ni, rescale=TRUE)
res = chisq.test(Ni, p=pi) # alternatively
res
curve(dchisq(x, df=df), xlim=c(0, 25))
curve(dchisq(x, df=df), xlim=c(res$statistic-2, res$statistic+2))
segments(res$statistic, 0, res$statistic, dchisq(res$statistic, df=df), col='red')

# Comparing two datasets

# continuous data
n = 100; m = 150
ks.test(rnorm(n), rnorm(m))
ks.test(rnorm(n), rexp(m))

# discrete data
n = 100; m = 150
data1 = rbinom(n, size=10, p=0.5)
data2 = rbinom(m, size=10, p=0.45)
bins = sort(unique(c(data1, data2)))
N1i = sapply(bins, function(i) sum(data1==i))
N2i = sapply(bins, function(i) sum(data2==i))
# built-in function
mat = cbind(N1i, N2i)
View(mat)
chisq.test(mat)
# manual calculation
#xi2 = sum((sqrt(m/n)*N1i - sqrt(n/m)*N2i)^2/(N1i + N2i))
#xi2
#df = length(bins)-1
#1-pchisq(xi2, df) # p-value

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
#### adult dataset from sdsR17.R
# execute script in a separate file
#source("dataprep.R", echo=T) 
# and save in R binary format
#save(data, file="sds17.Rdata") 

# or load previously saved data
load(file="sds17.Rdata")

# test covariate drift
ks.test(data$train$age, data$val$age)
# use correct=FALSE for no correction for continuity in 2x2 tables
chisq.test(table(data$train$sex), table(data$val$sex), correct=FALSE)

# see sds16.R for association measures

# testing independence
n = nrow(data$train)
U = table(data$train$sex, data$train$income)
View(U)
chisq.test(U, correct=FALSE) # Chi-squared test
library(DescTools)
# G-test
GTest(U) 
# G = 2*n*I(O, E)
2*n*MutInf(data$train$sex, data$train$income, base=exp(1))

# phi coefficient = pearson correlation
Phi(data$train$sex, data$train$income)
sqrt(chisq.test(U, correct=FALSE)$statistic/n)
cor(as.numeric(data$train$sex), as.numeric(data$train$income))

# V and T
CramerV(data$train$marital, data$train$income)
CramerV(data$train$race, data$train$income)
TschuprowT(data$train$marital, data$train$income)
TschuprowT(data$train$race, data$train$income)

# testing correlation
library(ISLR)
data(Carseats)
View(Carseats)
x = Carseats$Price
y = Carseats$Sales
plot(x,y)
shapiro.test(x) # univariate
shapiro.test(y) # univariate
library(mvnormtest)
U = rbind(x,y)
View(U)
mshapiro.test(U) # cannot reject bivariate normal
cor.test(x, y) # Pearson
cor.test(x, y, method='kendall') # Kendall's tau
cor.test(x, y, method='spearman') # Spearman's rho

### From sds31.R
library(caret)
noresampling = trainControl(method="none")
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", trControl=noresampling,
               # pass-trough options
               family=binomial(logit))
summary(lr.fit)
lr.pred = predict(lr.fit, newdata = data$val)
lr.prob = predict(lr.fit, newdata = data$val, type="prob") # as predict_proba in scikit-learn
lr.score = lr.prob[,2]  # score 
library(ROCR)
lr.prediction = prediction(lr.score, data$val$income)
lr.roc = performance(lr.prediction, "tpr", "fpr")
plot(lr.roc, colorize=T); abline(a=0, b= 1)
o = performance(lr.prediction,"auc")
o@y.values[[1]]

# Testing AUC-ROC - Using Wilcoxon (requires same shape)
negpos = split(lr.score, data$val$income)
n = length(negpos$`1`)
m = length(negpos$`0`)
S = function(s) sum(s > negpos$`0`) + sum(s == negpos$`0`)/2
U = sum(sapply(negpos$`1`, S))
U/(n*m) # equal to auc
U # same statistics used in wilcox.text
wilcox.test(negpos$`1`, negpos$`0`)
# Wilcox test assumes the distributions of score have the same shape. Have they?
featurePlot(x=data.frame(lr.score), y=data$val$income, plot='density', auto.key = list(columns = 2))

# Fligner-Policello test only assumes the distribution of the score are ymmetric. Are they?
# H0: AUC=0.5 H1: AUC>0.5
library(RVAideMemoire)
fp.test(negpos$`0`, negpos$`1`, alternative='greater') 

# Brunner-Munzel
# H0: AUC=0.5 H1: AUC>0.5
library(brunnermunzel)
# less=negatives rank lower than positives, i.e., AUC>0.5
brunnermunzel.test(negpos$`0`, negpos$`1`, alternative='less') 

# Other CI for AUC-ROC - DeLong
library(pROC)
ci.auc(data$val$income, lr.score) 

