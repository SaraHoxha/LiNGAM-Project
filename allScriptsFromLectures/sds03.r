######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 02 (ctd): Introduction and basics ##########

# functions: factorial
fact = function(x) if(x<=0) 1 else x*fact(x-1)
sapply(1:20, fact)

fact2 = function(x) prod(1:x)
sapply(1:20, fact2)

# Plot of Fig. 3.1 in [T]: no coincident birthdays
x = 1:100
pb = function(n) prod(1- (1:n-1)/365)
plot(x, sapply(x, pb), pch=16, cex=0.5)

# global variables
adda = function(x) { 
  x+a # global variable a
}
a=3; adda(8) # return 8+a
# local variables
add5 = function(x) { 
  a=5 # local variable, overrides global with same name
  x+a
}
a=3; add5(8)
a # a is still 3
# pass by value
change = function(x) { 
  x[1] = 100
  x
}
a=3; change(a)
a # a is unchanged
# same for vectors - try in Python, what does it happen?
w=c(0,1,2); change(w)
w # w is unchanged 

###### Script Lesson 03: Bayes's rule and applications ##########

# P(C|+) given P(C)
f = function(x) (.99*x)/(.99*x + .01*(1-x))
curve(f(x), xlim=c(0, 0.1), ylim=c(0,1), xlab='P(C)', ylab='P(C|+)', font.lab=2, lwd=2, col=2)

# Precision correction under shift
f = function(prec, gamma) prec/(prec + gamma*(1-prec))
gamma = c(seq(0.2, 0.8, 0.4), seq(1,5,2))
n = length(gamma)
for (i in 1:n)
  curve(f(x, gamma[i]), xlab='Prec', ylab='TruePrec', add=(i>1), font.lab=2, lwd=2, col=i)
legend("topleft", legend=paste("gamma=", gamma,sep=''), col=1:n, lty=1, lwd=2, box.lty=0, bg="transparent")
