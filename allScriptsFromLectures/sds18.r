######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 18: Estimators and MSE ##########

par(mar=c(4,4,1,1))

# Recall that for Exp(lambda)
# mu=1/lambda, sigma^2=1/lambda^2, median=ln(2)/lambda
lambda = 2
n = 1000

# CLT (for means)
o = replicate(1000, mean(rexp(n, lambda)))
plot(density(o))
mu = 1/lambda
abline(v=mu, col=2, lwd=2)
curve(dnorm(x, mu, 1/lambda/sqrt(n)), col=3, lwd=2, add=T)

# CLT (for medians)
o = replicate(1000, median(rexp(n, lambda)))
plot(density(o))
m = log(2)/lambda
abline(v=m, col=2, lwd=2)
curve(dnorm(x, m, 1/sqrt(4*n*dexp(m, lambda)^2)), col=3, lwd=2, add=T)

library(DescTools) # for FisherZ

# bivariate normal
snoise = 5; n = 20
f = function() {
  x = 3*rnorm(n)
  y = 2*x + 4 + rnorm(n, 0, snoise) # noise
  cor(x,y, method="pearson")
}
o = replicate(1000, f())
# true correlation (Exercise at home: prove it!)
rho = 1/sqrt(1+(snoise/6)^2)
rho
# E[R]
mean(o)
# tan(E[F(R)])
FisherZInv(mean(FisherZ(o)))
# FisherZ and FisherZInv
curve(FisherZ(x), n=1000, xlim=c(-1,1)) # n is number of points to plot
curve(FisherZInv(x), n=1000, xlim=c(-3,3)) 
# density of r
plot(density(o))
# density of F(r)
plot(density(FisherZ(o)), main=bquote(paste(rho, "=", .(rho))), xlab="F(r)")
abline(v=FisherZ(rho), col=3, lwd=2)
legend("topright", legend=bquote(paste("F(",rho,")")), col=3, lty=1, lwd=2, box.lty=0, inset=0.02)
curve(dnorm(x, FisherZ(rho), 1/sqrt(n-3)), col=2, lwd=2, add=T)

# try with different noise 
# try with non-bivariate normal

# compute pair of estimators (S, T)
st.est = function(data) 
{
  s = sum(data==0)/length(data)
  t = exp(-mean(data))
  return( c(s, t) )
}

n = 100
mu = log(10)
p0 = exp(-mu)
arrivals = rpois(n, mu)
arrivals
st.est(arrivals)
data = replicate(1000, st.est(rpois(n, mu)))
plot(density(data[2,]), col="blue", main='')
lines(density(data[1,]), col="red")
legend('topright', pch=16, c('S', 'T'), col=c('red', 'blue'))

# textbook: Fig. 19.4
et = function(n, mu) exp(-n*mu*(1-exp(-1/n))) # see Exercise 19.9
plot(1:n, et(1:n, mu), ylim=c(0, .2), pch=16)
abline(h=p0, col=2, lwd=2) # E[T]->p0  for n->infty

# Var(S) = p0*(1-p0)/n where p0 = exp(-mu)
var.s = function(n, mu) exp(-mu)*(1-exp(-mu))/n
plot(1:n, var.s(1:n, mu), ylim=c(0, .1), pch=16)
# Var(T) = E[T^2] - E[T]^2
var.t = function(n, mu) exp(-n*mu*(1-exp(-2/n))) - et(n, mu)^2
points(1:n, var.t(1:n, mu), ylim=c(0, 0.25), pch=16, col="red")

# [T]  textbook: Fig. 20.3
mse.s = function(n, mu) var.s(n, mu) + 0
mse.t = function(n, mu) var.t(n, mu) + (et(n, mu) - exp(-mu))^2
mus = seq(0, 5, 0.05)
plot(mus, mse.s(n, mus), pch=16, cex=0.5, col="red", xlab=expression(mu), ylab='MSE')
points(mus, mse.t(n, mus), pch=16, cex=0.5, col="blue")
legend('topright', pch=16, c('S', 'T'), col=c('red', 'blue'))
