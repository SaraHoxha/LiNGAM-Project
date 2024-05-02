######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 31: Two-sample tests of the mean and applications to classifier comparison

par(mar=c(4,4,1,1))

###### Classifiers in R ##########

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

# resampling method
noresampling = trainControl(method="none")

#### logistic regression
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", trControl=noresampling,
               # pass-trough options
               family=binomial(logit))
summary(lr.fit)

# predicted confidence on validation set
lr.pred = predict(lr.fit, newdata = data$val)
lr.pred
lr.prob = predict(lr.fit, newdata = data$val, type="prob") # as predict_proba in scikit-learn
View(lr.prob)
lr.score = lr.prob[,2] # scores
# built-in metrics
confusionMatrix(lr.pred, data$val$income, positive="1")
# other metrics
precision(lr.pred, data$val$income, relevant="1") # or PPV
recall(lr.pred, data$val$income, relevant="1") # or sensitivity
F_meas(lr.pred, data$val$income, relevant="1")
# Brier score
mean((lr.score - (as.numeric(data$val$income)-1))^2)
# calibration plot: P(Y=1|s(W) \in [a, b])
cal_data = calibration(data$val$income ~ lr.score, class="1")
plot(cal_data)

# manual computation of calibration plot
nbins=11
bins = seq(1/nbins, 1, 1/nbins)
id2bin = cut(lr.score, breaks=c(0,bins), labels=1:nbins)
bin.total = c(table(id2bin)) # c() to transform to vector
bin.pos = tapply(as.numeric(data$val$income)-1, id2bin, sum)
y = bin.pos/bin.total
x = ( c(0,bins[-nbins])+bins)/2 # midpoints
plot(x*100, y*100, type='o', xlim=c(0,100), ylim=c(0,100), 
     col="blue", xlab="Prediction Confidence", ylab="Perc. of Positives")
abline(coef = c(0,1),col="grey")
# add confidence interval
lines(cal_data$data$midpoint, cal_data$data$Lower, col="red")
lines(cal_data$data$midpoint, cal_data$data$Upper, col="red")
# exercise: how is $Lower and $Upper computed?

# binary-ECE
s_b = tapply(lr.score, id2bin, mean)
y_b = y
binECE = sum(bin.total*abs(y_b-s_b))/sum(bin.total)
binECE

# more details on predictions
featurePlot(x=data.frame(lr.score), y=data$val$income, plot='density', auto.key = list(columns = 2))
cutoff = 0.3
lr.pred.cutoff = factor(ifelse(lr.score>=cutoff, 1, 0), levels=levels(data$val$income))
confusionMatrix(lr.pred.cutoff, data$val$income, positive="1")

# using ROCR for metrics at cutoff
library(ROCR)
lr.prediction = prediction(lr.score, data$val$income)
# acc at cutoff
lr.acc = performance(lr.prediction, "acc"); plot(lr.acc)
# tpr at cutoff
lr.tpr = performance(lr.prediction, "tpr"); plot(lr.tpr)
# f1 at cutoff
lr.f1 = performance(lr.prediction, "f"); plot(lr.f1)
# roc curve
lr.roc = performance(lr.prediction, "tpr", "fpr")
plot(lr.roc, colorize=T); abline(a=0, b= 1)
# AUC: performance returns an object whose members (called 'slots') are accessed with '@'
o = performance(lr.prediction, "auc")
o@y.values[[1]]

# using ROC as metric
# need to use class names that are valid variable identifiers
lev = c("class0", "class1")
data$train$income2 = factor(data$train$income); levels(data$train$income2) = lev
data$val$income2 = factor(data$val$income); levels(data$val$income2) = lev
lr.pred2 = as.factor(lr.pred); levels(lr.pred2) = lev
dat = data.frame(obs=data$val$income2, pred=lr.pred2, 
                 class0=lr.prob[,1], class1=lr.prob[,2])
View(dat)
twoClassSummary(dat, lev)
# custom summary
custSummary = function(data, lev=NULL, model=NULL) {
  prediction = prediction(data$class1, data$obs)
  o = performance(prediction,"auc")
  auc = o@y.values[[1]]
  c(AUC = auc)
}
custSummary(dat, lev)

# repeated cross validation with custom summary function
# setting repeated cross-validation
rcv = trainControl(method="repeatedcv", repeats=5, number=10, 
                   classProbs = TRUE, summaryFunction=custSummary)
set.seed(42) # reproducibility of folds
lr2.fit = train(income2~age+education+occupation+sex+race, data = data$train, 
               method = "glm", trControl=rcv, metric="AUC", # custom metric
               # pass-trough options
               family=binomial(logit))
lr2.fit
lr2.fit$resample # details over 5x10 folds
lr2.folds = lr2.fit$resample[['AUC']]
mean(lr2.folds) # reported is mean performance
t.test(lr2.folds) # accuracy confidence interval

#### random forest
tunegrid = expand.grid(.mtry=3) # fixed value, no grid search
set.seed(42) # same folds as lr2.fit
rf.fit = train(income2~age+education+occupation+sex+race, data = data$train, 
               method = "rf", trControl=rcv, metric="AUC", tuneGrid=tunegrid,
               # pass-trough options
               ntree=15)
rf.fit
rf.fit$resample # details over 5x10 folds
rf.folds = rf.fit$resample[['AUC']]
t.test(rf.folds) # confidence interval
# which is better between lr and rf?

# check on validation data
rf.pred = predict(rf.fit, newdata = data$val) # class predict
rf.prob = predict(rf.fit, newdata = data$val, type="prob") # as predict_proba in scikit-learn
rf.score = rf.prob[,2]
# built-in metrics
confusionMatrix(rf.pred, data$val$income2, positive="class1")
# AUC
rf.prediction = prediction(rf.score, data$val$income2)
rf.roc = performance(rf.prediction,"tpr","fpr")
plot(rf.roc, add=T, col="red")
performance(rf.prediction,"auc")@y.values[[1]]
plot(calibration(data$val$income ~ rf.score, class="1"))

# check on fold data
boxplot(lr2.folds, rf.folds)

###### Equality of mean of two samples ##########

## Normal data, variance known
# 2-sample z-test
library(BSDA)
x = rnorm(100); y = rnorm(100, 3, 2)
z.test(x, y, sigma.x=1, sigma.y=2, conf.level=0.99)

## Normal data, variance unknown, same variance
# 2-sample t-test
plot(density(lr2.folds), xlim=c(0.7, 0.85))
lines(density(rf.folds),col=2)
# test of normality
shapiro.test(lr2.folds)
shapiro.test(rf.folds)
# same variance?
var(lr2.folds); var(rf.folds)
# F-test 
var.test(lr2.folds, rf.folds)
# F-test direct calculation
n = length(lr2.folds)-1; m = length(rf.folds)-1
curve(df(x, n, m), xlim=c(0,4), ylab="df")
f = var(lr2.folds)/var(rf.folds) # F-stat
f
segments(f,0,f,df(f, m, n), col="red")
pr = pf(f, n, m) 
2*min(pr, 1-pr)# pvalue

library(ISwR)
attach(energy)
energy
s = split(expend, stature)
s
plot(density(s$lean), xlim=c(5, 15))
lines(density(s$obese),col=2)
# test of normality
shapiro.test(s$lean)
shapiro.test(s$obese)
# F-test for normal data
var(s$lean); var(s$obese)
var.test(s$lean, s$obese) # or
var.test(expend~stature) 
# t-test
t.test(s$lean, s$obese, var.equal=T) # or,
t.test(expend~stature, var.equal=T) # "~" read as "described by"

## Normal data, variance unknown, different variance
# Welch t-test
plot(density(lr2.folds), xlim=c(0.7, 0.85))
lines(density(rf.folds),col=2)
t.test(lr2.folds, rf.folds, alternative="greater")

## General data, large sample
data1 = runif(200, -2, 2)
data2 = rnorm(100)
plot(density(data1), xlim=c(-2, 4), ylim=c(0,0.6))
lines(density(data2),col=2)
# large sample t-test
z.test(data1, data2, sigma.x=sd(data1), sigma.y=sd(data2))
t.test(data1, data2) # good approximation, anyway

## General data, locaton-shift
# Wilcoxon rank-sum test
wilcox.test(data1, data2)
wilcox.test(lr2.folds, rf.folds)

## General data, bootstrap t-test (equal/different variance, unpaired/paired)
library(MKinfer)
boot.t.test(lr2.folds, rf.folds, R=1000) # see help

# Paired tests
# paired t-test - lower p-value than unpaired
t.test(lr2.folds, rf.folds, paired=TRUE) # or
t.test(lr2.folds - rf.folds, mu=0) # one-sample t-test on differences
# paired Wilcoxon test
wilcox.test(lr2.folds, rf.folds, paired=TRUE)
