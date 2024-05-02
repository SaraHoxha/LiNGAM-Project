######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 33: Multiple-sample tests of the mean and applications to classifier comparison

par(mar=c(4,4,1,1))

# Multiple tests

# H0: mu = 0   H1: mu != 0  alpha = 0.05
# Multiple comparisons
n = 20; m = 2000; alpha = 0.05
# m/2 experiments with N(0, 1) + m/2 experiments with N(1, 1)
data = matrix( c(rnorm(n*m/2, mean=0),rnorm(n*m/2, mean=1)), nrow=n, ncol=m)
View(data)
# multiple t-test  
pvalues = sapply(1:m, function(i) t.test(data[,i], mu=0, conf.level=1-alpha)$p.value)
cmatrix = function(p, m, alpha) {
  fp = sum(p[1:(m/2)] < alpha)
  tn = sum(p[1:(m/2)] >= alpha)
  tp = sum(p[(m/2)+(1:(m/2))] < alpha)
  fn = sum(p[(m/2)+(1:(m/2))] >= alpha)
  res = matrix(c(fp, tn, tp, fn), nrow=2, ncol=2)
  rownames(res) = c('reject H0', 'do not reject H0')
  colnames(res) = c('H0 true', 'H1 true')
  cat('FPR', (fp/(fp+tn)), 'FNR', (fn/(fn+tp)), 'FDR', (fp/(fp+tp)), '\n')
  return(res)
}
# no correction
cmatrix(pvalues, m, alpha)
# Bonferroni correction - 
p_bonf = pmin(1, pvalues*m)
cmatrix(p_bonf, m, alpha)
# using built-in function
cmatrix(p.adjust(pvalues, "bonferroni"), m, alpha)
# Sidak correction
p_sidak = pmin(1, (1 - (1-pvalues)^m))
cmatrix(p_sidak, m, alpha)
# Benjamini & Yekutieli for controlling FDR: https://en.wikipedia.org/wiki/False_discovery_rate#Benjamini%E2%80%93Yekutieli_procedure
cmatrix(p.adjust(pvalues, "BY"), m, alpha)
# q-values
# for installation see https://www.bioconductor.org/packages/release/bioc/html/qvalue.html
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# BiocManager::install("qvalue")
library(qvalue)
qv = qvalue(pvalues)$qvalues
head(qv)
cmatrix(qv, m, alpha)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# load previously saved data
load(file="sds17.Rdata")

# Testing multiple samples: multiple linear regression

# Contrast a model 
library(ISLR)
fit = lm(Sales~Advertising+Price+Age+Education+ShelveLoc, data=Carseats)
summary(fit)
# to a null model with intercept only
fit0 = lm(Sales~1, data=Carseats) 
summary(fit0)
# F-statistics calculation
ys = Carseats$Sales
fit_ys = fitted(fit) # fitted values
res = resid(fit) #  same as Sales - fit_ys
n = nrow(Carseats)
k = 6 # number of regressors (after dummy encoding)
# H0: all coefficients are 0 
sse = sum(res^2)
sse # unexplained error
ssr = sum((fit_ys-mean(ys))^2)
ssr # explained error
f = ssr/sse*(n-k-1)/k
f
1-pf(f, k, n-k-1) # p-value
# recall fit
summary(fit)
# post-hoc test
library(multcomp) # multiple comparison library
ph = glht(fit)
# t-test as post-hoc but with adjusted p-value for multiple comparison
summary(ph) # same t-values, ma corrected p-values
# adjusted confidence intervals (stochastic method)
confint(ph, level=0.95)
# different from
confint(fit)

# Testing multiple samples: ANOVA
boxplot(Sales~ShelveLoc, data=Carseats)
# req: normality of datasets
tapply(Carseats$Sales, Carseats$ShelveLoc, shapiro.test)
# req: homogeneity of variances
bartlett.test(Sales~ShelveLoc, data=Carseats) # generalize F-test
# ANOVA via regression
fit = lm(Sales~ShelveLoc, data=Carseats)
summary(fit)
anova(fit)
ph = glht(fit, linfct = mcp(ShelveLoc = "Tukey"))
summary(ph) # Tukey all-pairs post-hoc
confint(ph, level=0.99) # confidence intervals
ph = glht(fit, linfct = mcp(ShelveLoc = "Dunnett"))
summary(ph) # Dunnett one-vs-other post-hoc
confint(ph, level=0.99) # confidence intervals

# Testing multiple samples: non-parametric test
# Package SCMAMP: see https://github.com/b0rxa/scmamp
# installation
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("b0rxa/scmamp")
# end installation
library(scmamp)
data(data_gh_2008)
dataset = data.gh.2008
View(dataset)
plotDensities(data=dataset, size=1.1)
boxplot(dataset)
# req: normality REJECTED => cannot use ANOVA
sapply(dataset, function(r) shapiro.test(r)$p.value)
# req: homogeneity of variances
merged = data.frame(acc=c(t(dataset)), clf=rep(colnames(dataset), nrow(dataset)))
View(merged)
bartlett.test(acc~clf, data=merged)
# Friedman non-parametric test
friedmanTest(dataset)
# Post-hoc Nemenyi 
test = nemenyiTest(dataset, alpha=0.05)
plotCD(dataset, alpha=0.05, cex=1.25)
test # critical difference 
test$diff.matrix # difference in mean ranks
abs(test$diff.matrix) > test$statistic # significant tests
