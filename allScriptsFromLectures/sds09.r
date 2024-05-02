######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 09: Expectation and variance ##########

# X ~ U(0, v) E[X] = v/2
width = runif(5000, 0, 10)
mean(width)
plot(ecdf(width), xlim=c(0,100))
area = width^2 # area of a square
mean(area) # E[g(X)] is NOT g(E[X])!
plot(ecdf(area), add=TRUE, col=2) # empirical CDF
curve(sqrt(x)/10, add=TRUE, col=3) # theoretical CDF

# Entropy of X ~ Ber(p)
H = function(p) - p*log2(p) - (1-p)*log2(1-p)
curve(H, xlim=c(0, 1))

# log(Y) ~ N(mu, sigma^2)
curve(dlnorm(x), xlim=c(0, 100))
curve(dlnorm(x), xlim=c(0.001, 1000), log="x")
curve(dnorm(log(x))/x, xlim=c(0.001, 1000), col="red", add=T)

# the area A = pi*R^2 for a uniform radius R is NOT uniform!
# pdf of A = g(R) = pi*R^2 for R ~ U(0, 1)
par(mar=c(4,4,1,1)) # margin c(bottom, left, top, right)
curve( 1/(2*sqrt(pi * x)), xlim=c(0, pi), ylim=c(0, 1.7))
# cdf of A = g(R) = pi*R^2 for R ~ U(0, 1)
curve( sqrt(x/pi), add=TRUE, col=2) # cdf
