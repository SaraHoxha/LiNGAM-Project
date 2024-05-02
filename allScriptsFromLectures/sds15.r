######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 15: Graphical summaries ##########
# See also: https://www.r-graph-gallery.com/

par(mar=c(4,4,1,1))

# load dataset
library(ISwR)
data(energy)
attach(energy)

# explicit
emp_cdf = cumsum(table(expend))/length(expend)
plot(names(emp_cdf), emp_cdf, type='o')
# built-in function ecdf()
plot(ecdf(expend))

# discrete data: rolling a die 1000 times
dataset = sample(1:6,1000,replace=T)
barplot(table(dataset)) # absolute
barplot(prop.table(table(dataset))) # relative
plot(as.factor(dataset)) # barplot is default for factors data types

# continuous data: sampling from normal distribution 1000 times
dataset = rnorm(100)
barplot(table(dataset)) # does not work: why?
hist(dataset)
hist(dataset, freq=F) # scaled to total area of 1
hist(dataset, freq=F, breaks=20) # number of bins
hist(dataset, freq=F, breaks=seq(-3, 3, 0.5)) # bin width
curve(dnorm(x), add=T, col="red", lwd=2) # generating density
rug(dataset) # a one-dimensional scatter plot

# Old Faithful Geyser Data
View(faithful)
erup = round(faithful$eruptions*60) # eruption duration in seconds
hist(erup, freq=FALSE, breaks=seq(90, 360, 2)) # Fig. 15.2 in [T] textbook
hist(erup, freq=FALSE, breaks=seq(90, 360, 90))
hist(erup, freq=FALSE, breaks=seq(90, 360, 20))

# Scott's normal reference method
b = 3.49 * sd(erup) * length(erup)^(-1/3)
hist(erup, freq=FALSE, breaks=seq(50, 350, b)) 
hist(erup, freq=FALSE, breaks="Scott") # Scott (but R rounds differently)

# other methods: https://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width
hist(erup, freq=FALSE, breaks="Sturges") # Sturges is default method in hist()
h = hist(erup, freq=FALSE, breaks="FD") # Freedman-Diaconis
h

#### Kernel Density Estimation

# Epanechnikov kernel 
Ke = function(t) pmax(0, 3/4*(1-t^2) ) 
curve(Ke(x), xlim=c(-3,3), ylim=c(-0.1,1.2))

# Triweight kernel 
Kt = function(t) pmax(0, 35/32*(1-t^2)^3)
curve(Kt(x), col=2, add=T)

# Normal kernel 
Kn = function(t) exp(-t^2/2)/sqrt(2*pi)
curve(Kn(x), col=3, add=T)

# scaled and shifted Epanechnikov kernel
# domain: [x-h, x+h]
scaleshiftKe = function(h, x, t) Ke((t-x)/h)/h 

# KDE procedure for 2 points
erup = sort(erup)
erup
x1 = erup[1]; x2 = erup[2]
x1; x2
h = 10
curve(scaleshiftKe(h, x1, x), xlim=c(x1-h-1, x2+h+1), ylim=c(0,0.15))
curve(scaleshiftKe(h, x2, x), col=2, add=T)
curve(scaleshiftKe(h, x1, x)+scaleshiftKe(h, x2, x), col=3, add=T)
curve((scaleshiftKe(h, x1, x)+scaleshiftKe(h, x2, x))/2, xlim=c(x1-h-1, x2+h+1), ylim=c(0,0.15))

# KDE density
hist(erup, freq=FALSE, ylim=c(0, 0.012))
lines(density(erup, kernel='epanechnikov', bw=h), col=2, lwd=2)
lines(density(erup, kernel='epanechnikov', bw=5), col=3, lwd=2)
lines(density(erup, bw=h), col=4, lwd=2) # default kernel is Gaussian

# choice of bandwidth h
normalh = (4/3)^(1/5)*sd(erup)*length(erup)^(-1/5) # Normal reference method
lines(density(erup, bw=normalh), col=5, lwd=2)

# plug-in density estimate best bandwidth
library(plugdensity)
hist(erup, freq=FALSE)
lines(density(erup, bw=h), col=2, lwd=2)
lines(plugin.density(erup), col=3, lwd=2)

# boundary kernels
data = rexp(1000)
plot(density(data)) # wrong for x <= 0
curve( dexp(x), add=T, col=2, lwd=2 )

library(ks) # kernel smoothing library
fhat = kde.boundary(data, h=1, xmin=0)
plot(fhat, lwd=2)
curve( dexp(x), add=T, col=2, lwd=2 )

data = rnorm(10000)
fhat = kde(data)
plot(fhat, lwd=2)
curve(dnorm(x), add=T, col=2, lwd=2)
# sampling from kde - see Lesson 12
samp = rkde(10000, fhat)
plot(kde(samp), col=3, lwd=2, add=T)

# multiplot
ee = split(expend, stature)
par(mfrow=c(1,2)) # grid of 1 row x 2 cols
hist(ee$lean,col="white")
hist(ee$obese,col="grey")
par(mfrow=c(1,1))

# scatter plots
attach(iris)
par(xpd=TRUE, mar=c(4,4,3,1)) # allow coordinates outside plot
plot(Sepal.Width, Sepal.Length, col=c("red","blue","green")[Species])
legend(2.5, 8.5, legend = levels(Species), col=c("red","blue","green"), pch=1, ncol=length(levels(Species)))
par(xpd=FALSE) # allow coordinates outside plot
coplot(Sepal.Length ~ Sepal.Width | Species ) #  y ~ x | a asks for plots of y versus x conditional on a
pairs(iris) # all pairs scatter plots

# bivariate density
plot(faithful$eruptions,faithful$waiting)
library(MASS)
image(kde2d(faithful$eruptions,faithful$waiting))
contour(kde2d(faithful$eruptions,faithful$waiting))
persp(kde2d(faithful$eruptions,faithful$waiting),phi=30,theta=50,xlab="duration",ylab="waiting time")

# Exercise at home
# The mnist dataset (mnist$train$x) contains 60K images of the MNIST database 
# as a matrix 60K x 768 where 768 = 28 x 28 is the number of pixels of each image. 
# Each pixel's greyscale color is a number between 0 (white) and 1 (black).
# Each image encode a digit from 0 to 9 stored in mnist$train$y.
#
# 1) Compute the average gray level (av_gray_one) for each image of the digit "1"
# 2) Compute and plot the kde of av_gray_one. Consider taking into account that 
#    it is a positive variable.

library("devtools")
devtools::install_github("xrobin/mnist") # see https://github.com/xrobin/mnist

library(mnist)
data(mnist)
show.digit(mnist$train$x[5,])
mnist$train$y[5]
