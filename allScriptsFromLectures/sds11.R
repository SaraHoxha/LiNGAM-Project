######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 11: Moments, functions of random variables ##########

# positive skewness and platykurtic
x = rbinom(10000, 50, 0.1)
plot(table(x)/length(x), type='p')
mu = mean(x) # sample mean
sigma = sd(x) # sample sd
mu3 = mean((x-mu)^3)/sigma^3 # sample skewness
mu3
# or using a library
library(moments)
skewness(x)
kurtosis(x)

# set working dir to current source file
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Data from Human Mortality Database https://www.mortality.org (require registration)
#library(HMDHFDplus)
#mortalitycount = readHMDweb(CNTRY='ITA', item='Deaths_1x1', username='<username>', password='<pwd>')
#save(mortalitycount, file="smd11.Rdata")
load(file="sds11.Rdata")
View(mortalitycount)

# negative skewness and leptokurtic
year = 2015
ndeaths = mortalitycount[mortalitycount$Year==year,]$Total
ages = mortalitycount[mortalitycount$Year==year,]$Age
plot(ages, ndeaths)
x = rep(ages, ndeaths) # sample from counts
mean(x)
plot(table(x)/length(x), xlab="ages", ylab="pmf")
skewness(x)
kurtosis(x)

### cross-entropy and KL divergence
# random data
data = rgeom(1000, 0.7)
# empirical distribution
px = table(data)/length(data) 
px

kldist = function(p) {
  # theoretical distribution
  values = as.numeric(names(px))
  pmf = dgeom(values, p) 
  py = pmf/sum(pmf) # normalize pmf to values
  # KL calculation
  cross_entropy = -sum(px*log(py))
  entropy = -sum(px*log(px))
  kl = cross_entropy - entropy
  kl
}

kldist(0.7)
p = seq(0.6, 0.8, 0.01)
plot(p, sapply(p, kldist)) # minimize kl iff minimize cross_entropy

# normalized mutual information
nmi = function(data1, data2) {
  px = table(data1)/length(data1)
  py = table(data2)/length(data2)
  pxy = table(data1, data2)/length(data2)
  pxy = pxy[pxy>0] # avoid 0*log(0)
  hx = -sum(px*log(px))
  hy = -sum(py*log(py))
  hxy = -sum(pxy*log(pxy))
  (hx+hy-hxy)/min(hx, hy)
}

# load iris
data(iris)
x = cut(iris$Sepal.Length, breaks=4:8); x
nmi(x, iris$Species) # nmi as a feature selection measure
x = cut(iris$Sepal.Width, breaks=1:5); x
nmi(x, iris$Species) # nmi as a feature selection measure

library(aricode)
NMI(x,iris$Species, variant="min") # variants in normalizations
