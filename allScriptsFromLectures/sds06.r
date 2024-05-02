######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 06: Recalls on calculus ##########

# polynomials
curve(3*x, xlim=c(-15, 15), ylim=c(-100, 100))
curve(x^2, col=2, add=T)
curve(x^3, col=3, add=T)
curve(x^4, col=4, add=T)
legend('top', legend=c('3x', 'x^2', 'x^3', 'x^4'), lty=1, col=1:4,
       inset=c(0, -0.3), xpd=TRUE, ncol=4) # put legend outside plot

# powers and logarithms
a = 2

a^3 * a^2 # a^m * a^n = a^(m+n) 
a^3 / a^2 # a^m / a^n = a^(m-n) 
a^0 # a^0 = a^m / a^m = 1
a^(-2) # a^(-m) = a^0 / a^m = 1/a^m
(a^2)^3 # (a^m)^n = a^(m*n) 

b = 8
4^(1/2) # is sqrt(4) - the principal square root of 4 (there are 2 roots: -2 and +2)
(-4)^(1/2) # sqrt(-4) does not exist
curve(2^x, xlim=c(-10,10))
curve(0.1^x, col=2, add=T)

exp(1) # euler number e, exp(a) = e^a
exp(3) # e^3
curve(exp(x), xlim=c(-1,10))
k = 0:10
# approximation of Taylor series
g1 = function(x) sum(x^k/factorial(k))
g = Vectorize(g1) # vectorize function over vector
curve( g, col=2, add=T)
k = 0:20 # better approximation
curve( g, col=3, add=T)

log(exp(3)) # log(a) = b such that e^b = a, for a >= 0
log(1)
curve(log(x), xlim=c(0, 10))

log(a*b) # log(a*b) = log(e^log(a)*e^log(b)) = log(e^(log(a)+log(b)) = log(a)+log(b)
log(a) + log(b)
log(a/b) # log(a/b) = log(e^log(a)/e^log(b)) = log(e^(log(a)-log(b)) = log(a)-log(b)
log(a) - log(b)
log(a^3) # log(a^k) = k log(a)
3*log(a)
log(a^3, base=a) # log_a(b) = v such that a^v = b
log(a^3)/log(a) # a^log_a(b) = b = e^log(b) AND a^log_a(b) = e^(log(a)*log_a(b) ) IMPLY log(b)/log(a) = log_a(b)

# linear functions
curve(3*x+2, ylim=c(-3,7)) # positive gradient (or slope)
curve(-3*x+3, col=2, add=T) # negative gradient (or slope)
curve(0*x + 2, col=3, add=T) # zero gradient - constant functions

# gradient of x^2
curve(x^2, xlim=c(-15, 15), ylim=c(-10, 100))
# tangent at x=a has gradient 2*a and offset = a^2 - 2*a*a = -a^2 
curve(10*x - 25, col=2, add=T) # e.g., a=5
for(a in -10:10) {
  curve( 2*a*x + (a^2 - 2*a*a), col=3, add=T)
}

# derivatives
dx3 = D(expression(x^3), "x") # symbolic derivatives
dx3
class(dx3) # callable object type
eval(dx3, list(x=5)) # eval dx3 at x=5
x = seq(-5, 5, 0.5)
plot(x, eval(dx3), xlim=c(-5, 5))
fdx3 = function(x) eval(dx3, list(x=x)) # eval dx3 at any x passed to function
curve(fdx3, from=-5, to=5)

D(dx3, "x") # second derivative
D(expression(x^n), "x") # partial derivative d x^n / dx 
D(expression(x^n), "n") # partial derivative d x^n / dn

# gradient of f(x) using f'(x)
f = function(x) x^3+4*x^2 # replace body with any expression on x, eg. x^3
fprime = function(x) eval(D(body(f), "x"))
curve(f, xlim=c(-5, 5), ylim=c(-10, 10))
for(a in -5:5) 
  curve( fprime(a)*x + (f(a) - fprime(a)*a), col=3, add=T)

# minimum using derivatives (no conclusion, we should look a 4th derivative)
root = uniroot(fprime, c(-1,1))$root # find root of fprime
root
fprime(root)
fsecond = function(x) eval(D(D(body(f), "x"), "x"))
fsecond(root)

# minimum using numeric methods
optimize(f, c(-1000, 1000))

# definite integrals computed using numeric methods (with approximation)
ee = function(x, lambda=2) x*lambda*exp(-lambda*x)
integrate(ee, 0, Inf) # Expectation of exponential distribution: 1/lambda

# Euler's gamma(x) function extends factorial(x-1)
plot(0:10, factorial(0:10), log='y')
curve(gamma(x+1), add=T, col='red')
