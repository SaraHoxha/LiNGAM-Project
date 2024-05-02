######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 13: Powerlaws, Pareto laws, Zipf laws ##########

#install.packages("poweRlaw") # install a package
# Reference for powerlaw & lognormal estimation
# Package: https://cran.r-project.org/web/packages/poweRlaw/poweRlaw.pdf
library(poweRlaw)

# parameters
xmin = 1
alpha = 2.5
### continuous powerlaw

par(mar=c(4,4,1,1))

# density
curve(dplcon(x, xmin, alpha), xlim=c(1,20), lwd=2)

# vs Exp(lambda), for lambda = C, so that exp starts as the powerlaw
lambda = dplcon(xmin, xmin, alpha) # density at x=xmin
lambda
# exp(x-xmin, lambda) = lambda e^{-lambda (x-xmin)} (x-xmin since exp starts at 0)
curve(dexp(x-xmin, lambda), col='red', add=T, lwd=2)
# and in log-log scale
curve(dplcon(x, xmin, alpha), xlim=c(1,200), ylim=c(0.000005, 3), log="xy", lwd=2) 
curve(dexp(x-xmin, lambda), col='red', add=T, lwd=2)
# vs LogNorm
curve(dlnorm(x-xmin, 0, 1.3), col=3, lwd=2, add=T)

# ccdf
curve(dplcon(x, xmin, alpha), xlim=c(1,1000), ylim=c(1e-8, 2), log="xy")
legend("topright", legend=c('pdf', 'ccdf', 'pdf(alpha-1)'), col=c(1, 2, 3), lty=1)
curve(1-pplcon(x, xmin, alpha), col=2, log="xy", add=T) 
# ccdf(alpha) = x^{-(alpha-1)}/dplcon(xmin, xmin, alpha-1)
curve(dplcon(x, xmin, alpha-1)/dplcon(xmin, xmin, alpha-1), col=3, add=T) 

# VGAM package provide pareto distribution (over beta)
#install.packages("VGAM") 
library(VGAM) # attach (load) package

beta = 1.5
# dpareto(beta) = dplcon(alpha=beta+1)
curve(dpareto(x, xmin, beta), xlim=c(1,1000), ylim=c(1e-8, 2), log="xy")
legend("topright", legend=c( expression(paste('pareto(', beta, ')')),
                             expression(paste('plcon(', beta, '+1)')),
                             expression(paste('ccdf pareto(', beta, ')')),
                             expression(paste('plcon(', beta, ')')) ), 
       col=c(1, 2, 3, 4), lty=1)
curve(dplcon(x, xmin, beta+1), col=2, add=T) 
# ccdf pareto(beta) = dplcon(beta) up to a constant
curve(1-ppareto(x, xmin, beta), col=3, add=T) 
curve(dplcon(x, xmin, beta)/dplcon(xmin, xmin, beta), col=4, add=T) 

# discrete powerlaw
kmin = 1
k= kmin+(0:1000)
plot(k, dpldis(k, xmin, alpha), xlim=xmin+c(1,1000), log="xy") # log-log scale
plot(k, ppldis(k, xmin, alpha), xlim=xmin+c(1,1000), log="xy") # powerlaw cdf
plot(k, 1-ppldis(k, xmin, alpha), xlim=xmin+c(1,1000), log="xy") # powerlaw ccdf

# Riemann zeta function zeta(x) = sum( (1:Inf)^(-x) )
curve(zeta(x), xlim=c(1,1.5))
zeta(2.5)
sum( (1:10000)^(-2.5) )

# realizations of powerlaw - continuous data
n = 10000
rpl = rplcon(n, xmin, alpha) # random generation
m_pl = conpl(rpl) # built a model
plot(m_pl) # plot empirical **ccdf**
curve( (x/xmin)^(-alpha+1), add=T, col="red", lwd = 2) # add true ccdf

# realizations of powerlaw - discrete data
n = 100000
rpl = rpldis(n, kmin, alpha) # random generation
epmf = table(rpl)/n
plot(names(epmf), epmf, log="xy") # naive plot (see Fig. 3 of Newman paper)
curve( x^(-alpha)/zeta(alpha), add=T, col="red", lwd = 2) # add true pmf
m_pl = displ(rpl) # built a model
plot(m_pl) # plot **ccdf**
curve( (x/kmin)^(-alpha+1), add=T, col="red", lwd = 2) # add true ccdf (continuous approximation)

# zipf's law
N = 1000 # e.g., books
alphaz = 0.7 
C = 100 # e.g., reader of top book
x = C*(1:N)^(-alphaz) # e.g., readers per book
plot(1:N, x, log="xy", xlab="rank", ylab="abs. frequencies")
pmf = x/sum(x)
plot(1:N, pmf, log="xy", xlab="rank", ylab="pmf")
curve( x^(-alphaz)*pmf[1], add=T, col="red", lwd = 2) # pmf[1] because pmf[1] = p(1) = C * 1^-beta = C 

# zipf from powerlaw (see also http://www.hpl.hp.com/research/idl/papers/ranking/ranking.html)
ranked = sort(rpl, decreasing=T) # random discrete power law
ranked
plot(1:n, ranked, log="xy") # plot abs freq by rank
beta = 1/(alpha-1)
C = ranked[1]
curve( C*x^(-beta), add=T, col="red", lwd = 2)
pmf = ranked/sum(ranked)
plot(1:n, pmf, log="xy")
C = pmf[1]
curve( C*x^(-beta), add=T, col="red", lwd = 2)

# power-law from zipf 
data(moby) # word frequency in Moby Dick novel, already sorted decreasing
moby
N = length(moby)
plot(1:N, moby, log="xy", xlab="rank of a word", ylab="frequency")
# build a power-law
m_pl = displ(moby)
dd = plot(m_pl, ylab="CCDF") # plot **ccdf** and return plotted data
plot(dd$x, N*dd$y, log="xy", xlab="frequency", ylab="N P(X > frequency)")
points(moby, 1:N, col="red")

# power-law from zipf
N = 10000
beta = 1.2
C = 1000
data = C*(1:N)^(-beta)
plot(1:N, data, log="xy") # plot zipf
curve( C*x^(-beta), add=T, col="red", lwd = 2)
# build a power-law
m_pl = conpl(data) 
plot(m_pl) # plot **ccdf**
alpha = 1/beta + 1
xmin = min(data)
curve( (x/xmin)^(-alpha+1), add=T, col="red", lwd = 2)

# Powerlaw in Python (Alstott-Bullmore-Plenz)
# Paper: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0085777 
# Code: https://github.com/jeffalstott/powerlaw
###########
