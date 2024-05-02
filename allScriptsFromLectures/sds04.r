######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 04: Discrete Distributions ##########

# outer loops: sum of two dices
sum_dices = outer(1:6, 1:6, "+")
sum_dices

# compute absolute frequencies of values
afreq = table(sum_dices)
afreq
# probability mass function
pmf_sum = afreq/sum(afreq) 
pmf_sum
sum(pmf_sum)
plot(pmf_sum)
# cumulative distribution function
cdf_sum = cumsum(pmf_sum)
cdf_sum
plot(cdf_sum) # partially correct
plot(ecdf(sum_dices)) # correct (ecdf = empirical cumulative distribution function)
ecdf(sum_dices)(3.5) # ecdf(x) is a function!

# DISCRETE DISTRIBUTIONS

m = 18; M = 30
x = m:M 
pmf = table(x)/length(x)
pmf
plot(pmf, ylim=c(0,0.12)) # uniform discrete pmf
plot(ecdf(x)) # uniform discrete cdf
sample(x, 5, replace=TRUE) # sampling 5 elements

d = 1:9 # leading digit
dben = log10(1+1/d) # Benford's law
plot(d, dben, type='h') # pmf
dben
dataset = 2^(0:100) # dataset of 2's powers
firstd = as.numeric(substr(dataset, 1, 1)) # extract first digit
table(firstd)/length(firstd) # empirical frequencies

p = 0.75 # unfair coin
k = c(0, 1)
bernoulli = p^k*(1-p)^(1-k)
bernoulli
plot(k, bernoulli, type='h', ylim=c(0,1)) # bernoulli pdf
plot(k, dbinom(k, 1, p), type='h', ylim=c(0,1)) # dbinom R function for pmf
plot(k, pbinom(k, 1, p), type='h', ylim=c(0,1)) # pbinom R function for cdf
rbinom(5, 1, p) # sampling 5 elements

# maximum of two dices
max_dices = outer(1:6, 1:6, pmax)
max_dices
# pmax is the (parallel) maximum over pairs
pmax(c(1, 4), c(2, 3))
# probability mass function
pmf_max = table(max_dices)/36
pmf_max
plot(pmf_max)
# joint distribution
jpmf = table(sum_dices, max_dices)
jpmf
jpmf = jpmf/36
jpmf
# marginal of sum
sum_pmf = rowSums(jpmf)
sum_pmf # equal to
pmf_sum
# marginal of max
max_pmf = colSums(jpmf)
# check with pmf (up to numeric approximation)
max_pmf # equal to
pmf_max
# independence?
jpmf['8', '1']
sum_pmf['8']*max_pmf['1']

p = 0.5 # fair coin
n = 10 # number of trials
k = 0:10 # sum of successes
choose(n, k) # binomial coefficient
binomial = choose(n, k) * p^k * (1-p)^(n-k) # binomial distribution
plot(k, binomial, type='h') # from now on, we use points: pch=16 filled circle
plot(k, dbinom(k, n, p), pch=16) # dbinom R function for pmf

k = 1:10
geometric = p*(1-p)^(k-1) # geometric distribution
geometric[4] # e.g., probability of obtaining heads after 3 tails
dgeom(3, p) # built-in function ATTENTION: k-1 and not k 
plot(k, geometric, pch=16)
plot(k, dgeom(k-1, p), pch=16) # dgeom R function for pmf 

n = 10 # number of successes
k = 0:50 # number of failures before n successes
p = 0.5
plot(k, dnbinom(k, n, p), pch=16) # dnbinom R function for pmf

k = 0:20
lambda = 10 # it is called mu in the textbook [T]
plot(k, dpois(k, lambda), pch=16, ylim=c(0, .45)) # dpois R function for pmf
lambda = 5
points(k, dpois(k, lambda), pch=16, col='red') # add to existing plot
lambda = 0.9
points(k, dpois(k, lambda), pch=16, col=3) # add to existing plot
legend('topright', legend=c('lambda=10', 'lambda=5', 'lambda=0.9'), col=c(1, 2, 3), pch=16)
