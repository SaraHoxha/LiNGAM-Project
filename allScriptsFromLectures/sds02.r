######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 02: Introduction and basics ##########

# What is R? https://en.wikipedia.org/wiki/R_(programming_language) 
# CRAN https://cran.r-project.org/ 
# RStudio https://www.rstudio.com/ 

# An interpreted expression language (case sensitive)
2+2
3*NA # NA is missing
1/0 # Inf
0/0 # NaN
exp(2) # basic functions
2^4 # power
log(4) # default base is exp(1)

# Variables and assignments (Global Environment in RStudio or ls())
x <- 2 # or 
3 -> x
y = 2
x+x
s = sqrt(2)
s*s
s*s == 2 # FALSE
s*s - 2 # small difference due to finite-precision arithmetic
!TRUE
TRUE | FALSE
TRUE & NA # 3-valued logic
# Reserved/better not to use names: c,q,t,C,D,F,I,T

# Vectors, recycling and vector arithmetic- c() stands for "concatenate"
weight <- c(60, 72, 57, 90, 95, 72)
weight # [k] in the output means the index of the first element in the line
a = weight/2
a
a = weight/c(1,2) # recycling
a
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2
bmi

xbar <- sum(weight)/length(weight) # mean
mean(weight) # mean function
weight - xbar
sqrt(sum((weight - xbar)^2)/(length(weight) - 1)) # sd
sd(weight) # sd function
example(mean) # example usage of mean (or other R functions)

# Graphics, parameters: positional and named, default
plot(height, weight) # https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/plot
plot(height, weight, pch=16)
plot(y=weight,x=height)

# Scalar/vector types
a = 2
a = 'mario'
c(FALSE, 3) # upcasting
c(FALSE, 'mario', 3)
a = c("mario", 'rossi') # strings with " and ' delimiters
cat(a, sep=',') # output with concatenation
bmi > 25 # booleans

#  Create vectors
x <- c(1, 2, 3)
y <- c(10, 20)
z <- c(x, y, 5) # inner concatenation
z
x
x[5] = 5 # extend vector 
x
x[4] # will be NA
c(1) # is the scalar 1 

# named vectors 
x <- c(red="Huey", blue="Dewey", green="Louie") 
x["red"]
names(x)
names(x) = c() # discard names
x

#  vector access
z
z[1] # first element position is *1*
z[2:4] # last position is *included*
z[c(1, 4)] # elements 1 and 4
z[-1] # *except* first element
z[c(-1, -4)]
z>5 # vector of boolean
z[z>5] # conditioning
which(z>5) # positions whose values satisfy a condition
z[2*1:(length(z)/2)] # only even positions
z[z>=5 & z<=10] # vectorized AND of conditions

# seq ("sequence"), is used for equidistant series of numbers
4:10
seq(4,10) # or 
seq(4,10,2) # in Python 4:10:2
# rep ("replicate")
rep(1,5)
rep(1:2,2)
rep(c(1,2),c(10,15))

# Arrays are vectors with a given dimensionality 
# (1 = unidimensional, 2 = matrix, 3, .)
z
dim(z) # as Python shape
dim(z) = c(2, 3)
dim(z)
z
z[1,] # first row
z[,1] # first col
z[1,2]

z = array( 1:12, c(3,4) )
z
x= matrix(1:12,nrow=3,byrow=T)
x
rownames(x) <- LETTERS[1:3]
x
x["A",]
t(x)  # transposed
y = c(10, 20, 30)
cbind(x, y)
rbind(x, seq(10, 40, 10))
# matrix index
x <- array(1:20, dim=c(4,5))
x
x[1,3]
i <- array(c(1:3,3:1), dim=c(3,2))
i
x[i]

# Operators on arrays - not done - see [B2] Sect. 5.7 on matrix facilities
# %o% # outer product
# %*% # matrix multiplication
# crossprod(x, y) # is t(X) %*% y
# diag(M)
# solve(A,b)

# Factors = categorical values (in Python categorical)
pain <- factor(c("none","severe","medium","medium","mild")) # categorical values - nominal
pain
levels(pain)
cut( 1:10, breaks = c(0, 3, 10)) # discretization into intervals

# lists = ordered collection of objects of possibly different type
intake.pre <- c(5260,5470,5640,6180,6390, 6515,6805,7515,7515,8230,8770)
intake.post <- c(3910,4220,3885,5160,5645, 4680,5265,5975,6790,6900,7335)
mylist <- list(intake.pre, intake.post, "mario")
mylist
# list with name
mylist <- list(before=intake.pre,after=intake.post)
mylist

# list with name, close to a dictionary
Lst <- list(name="Fred", wife="Mary", no.children=3, child.ages=c(4,7,9))
Lst
Lst[[1]] # or 
Lst[["name"]] # or 
Lst$name
Lst[1] # sublist
Lst[c(1, 2)]
length(Lst)
append(mylist, Lst) # list appending

# data frame = dataset = table = list of vectors or factors of the same length
d <- data.frame(intake.pre,intake.post)
d
length(d)
nrow(d)
d[1,2] # access element
d[c(3, 6),1] # access elements
d[5,] # access whole row
d[1:6,] # access whole rows 
head(d) # first 6 rows
d[1] # sub-frame with first column
d[c(1,1,2)] # or with selected columns
d[[1]] # first column as a vector
d[,1] # or
rownames(d)
colnames(d)
d$intake.pre = NULL # remove element
d

# complex numbers
w = 5 + 1i;
2i*w

# data summaries, object class
summary(height)
class(height)
summary(z)
class(z)
summary(d)
class(d)
class(Lst)
class(w)

# implicit loops
lapply(mylist, mean)
sapply(mylist, mean)
