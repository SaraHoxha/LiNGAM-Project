######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 23: Statistical decision theory #######

par(mar=c(4,4,1,1))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#### adult dataset from sds17.R
# execute script in a separate file
#source("dataprep.R", echo=T) 
# and save in R binary format
#save(data, file="sds17.Rdata") 

# or load previously saved data
load(file="sds17.Rdata")
View(data)

# using caret (Classification and Regression Training)
# https://topepo.github.io/caret/available-models.html
library(caret)

# models and parameters available
View(modelLookup())
# logistic regression
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", family=binomial(logit))
lr.fit
summary(lr.fit)

# predicted confidence on validation set
lr.pred = predict(lr.fit, newdata = data$val)
lr.pred
lr.prob = predict(lr.fit, newdata = data$val, type="prob") # as predict_proba in scikit-learn
View(lr.prob)
# built-in metrics
confusionMatrix(lr.pred, data$val$income, positive="1")

# predict class for binary caret classifiers
predict.caret = function(object, newdata, ...) {
  predict(object, newdata, type='prob')[,2] >= 0.5
}

# adapted: https://rpubs.com/ZheWangDataAnalytics/DecisionBoundary
# input: 
# model: classification model
# data: training set
# class: response variable
boundary <- function(model, data, prediction = predict.caret, class = NULL, 
                     resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- prediction(model, g)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

# iris dataset
data(iris)
# only use two predictors
x = iris[, c("Sepal.Length", "Sepal.Width", "Species")]
plot(x[,1:2], col = x[,3])
x["Class"] = as.factor(ifelse(x["Species"]=="setosa", 1, 0))

# LR
model1 = glm(Class ~ Sepal.Length + Sepal.Width, data = x, family=binomial(link='logit'))

predict.lr = function(object, newdata, ...) {
  predict.glm(object, newdata, type = "response") >= 0.5
}

boundary(model1, x, predict.lr, class="Class", main="LR")
coef = coef(model1)
coef
abline(b=-coef[2]/coef[3], a=-coef[1]/coef[3], lwd=2, col=4)

# LR with caret
model2 = train(Class ~ Sepal.Length + Sepal.Width, data = x, 
               method="glm", family=binomial(logit))

boundary(model2, x, class="Class", main="LR with caret")

# non linear sample, perfectly separable
samplenonlinear = function(n) {
  x = runif(n, 0, 1)
  y = runif(n, 0 ,1)
  c = as.factor(ifelse(x^2 + y^2 < .64, 1, 0))
  y = y + rnorm(n, 0, 0.05) # some noise
  data.frame(x,y,c)
}

df_train = samplenonlinear(1000)
df_test = samplenonlinear(200)
lr.fit = train(c~x+y, data=df_train, method="glm", family=binomial(logit))
boundary(lr.fit, df_test, class="c", main = "LR")
curve(sqrt(.64-x^2), n=1000, add=T, col=4, lwd=2, xlim=c(0,.7999))

knn.fit = knn3(c~x+y, data=df_train, k=5)
boundary(knn.fit, df_test, class="c", main="kNN (k=5)")
curve(sqrt(.64-x^2), n=1000, add=T, col=4, lwd=2, xlim=c(0,.7999))

# bias-variance trade-off
c = as.numeric(df_test$c)-1
bvtradeoff = function(k) {
  knn.fit = knn3(c~x+y, data=df_train, k=k)
  p =  predict(knn.fit, df_test, type='prob')[,2]
  c(var(p), mean(p-c)^2)
}
ks = seq(10, 400, 10)
bv = sapply(ks, bvtradeoff)
plot(ks, bv[2,], ylim=c(1e-08, .25), log='y', xlab='k', ylab='bias^2 and var')
points(ks, bv[1,], col=2)

# margin-based loss functions
curve(x<=0, xlim=c(-1, 3), ylim=c(0, 4), lwd=2, col=2, xlab="m", ylab="loss")
legend("topright",c("0-1", "L2","Logistic-loss", "Hinge-loss", "Exp-loss"), col=2:6, lwd=2)
curve((1-x)^2, lwd=2, col=3, add=T)
curve(log(1+exp(-x), base=2), lwd=2, col=4, add=T)
curve(pmax(0,(1-x)), lwd=2, col=5, add=T)
curve(exp(-x), lwd=2, col=6, add=T)

# Bayes optimal decision boundary
eta = function(x, y) {
  # f(1|x,y) = 1 for x^2+y^2<.64 and 0 o.w.
  ifelse(x^2 + y^2 < .64, 1, 0)
}
bayesopt = Vectorize(eta)
predict.bayesopt = function(object, newdata, ...) {
  object(newdata$x, newdata$y) >= 0.5
}
boundary(bayesopt, df_test, predict.bayesopt, class="c", main="Bayes Opt")
curve(sqrt(.64-x^2), n=1000, add=T, col=4, lwd=2, xlim=c(0,.7999))

# Next example adapted from: https://www.r-bloggers.com/2020/01/optimal-decision-boundaries/ 

library(mvtnorm) # multivariate normal density

mux0 = 0; muy0 = 2
mux1 = 2; muy1 = 0
covmat0 = matrix(c(1, 0.3, 0.3, 1), nrow=2, byrow=TRUE)
covmat1 = matrix(c(1, -0.3, -0.3, 1), nrow=2, byrow=TRUE)
fbiv0 = function(x, y) dmvnorm( c(x,y), c(mux0, muy0), covmat0)
fbiv1 = function(x, y) dmvnorm( c(x,y), c(mux1, muy1), covmat1)

# non linear sample, non perfectly separable
samplenonsep = function(n) {
  # C ~ Ber(0.5)
  n0 = rbinom(1, n, 0.5)
  n1 = n - n0
  # P(X|C=0) ~ Bivariate Normal
  xy0 = rmvnorm(n0, c(mux0, muy0), covmat0)
  df0 = data.frame(xy0)
  names(df0) = c("x", "y")
  df0$c = 0
  # P(X|C=1) ~ Bivariate Normal
  xy1 = rmvnorm(n1, c(mux1, muy1), covmat1)
  df1 = data.frame(xy1)
  names(df1) = c("x", "y")
  df1$c = 1
  res = rbind(df0, df1)
  res$c = as.factor(res$c)
  res
}

df_train = samplenonsep(1000)
df_test = samplenonsep(200)
lr.fit = train(c~x+y, data=df_train, method="glm", family=binomial(logit))
boundary(lr.fit, df_test, class="c", main = "LR")

knn.fit = knn3(c~x+y, data=df_train, k=5)
boundary(knn.fit, df_test, class="c", main="kNN (k=5)")

# Bayes optimal decision boundary
eta = function(x, y) {
  # f(1|x,y) = f(x,y|1)f(1)/[f(x,y|0)f(0)+f(x,y|1)f(1)]
  fbiv1(x, y) / (fbiv0(x, y) + fbiv1(x, y))
}
bayesopt = Vectorize(eta)
#bayesopt(df_test$x, df_test$y)
boundary(bayesopt, df_test, predict.bayesopt, class="c", main="Bayes Opt")

# selective classification
lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", family=binomial(logit))
lr.pred = predict(lr.fit, newdata = data$val)
confusionMatrix(lr.pred, data$val$income, positive="1")
lr.prob = predict(lr.fit, newdata = data$val, type="prob") # as predict_proba in scikit-learn
View(lr.prob)
g = function(prob, tau) tau > pmin(prob[1], prob[2])
taus = seq(0.05, 0.5, 0.05)
coverage = c()
misc = c()
brier = c()
imprec = c()
for (tau in taus) {
  sel = g(lr.prob, tau)
  cov0 = mean(sel)
  coverage = append(coverage, cov0)
  cm = confusionMatrix(lr.pred[sel], data$val$income[sel], positive="1")
  risk0 = 1-cm$overall["Accuracy"]
  misc = append(misc, risk0)
  truey = as.numeric(data$val$income[sel])-1
  risk1 = mean((lr.prob[sel,2]-truey)^2) # 1-cm$byClass["Precision"]
  brier = append(brier, risk1)
  risk2 = 1-cm$byClass["Precision"]
  imprec = append(imprec, risk2)
}
# coverage-risk curve for misclassification error
plot(coverage, misc, type='b', ylim=c(0,.25))
# add tau as labels: pos=3 on top of points
text(coverage, misc, labels=taus, cex=0.9, font=2, pos=3) 

# coverage-risk curve for Brier-score (or L2)
plot(coverage, brier, type='b', ylim=c(0,.15))
text(coverage, brier, labels=taus, cex=0.9, font=2, pos=3) 

# coverage-risk curve for imprecision
plot(coverage, imprec, type='b', ylim=c(0,.5))
text(coverage, imprec, labels=taus, cex=0.9, font=2, pos=3) 
